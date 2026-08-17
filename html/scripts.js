window.addEventListener("load", () => {
  const url = new URL(window.location.href);
  let workspace = repository + document.getElementById("script_workspace").value;
  let prefix_for_examples = workspace + "/";
  let list_path = prefix_for_examples + "list.json";
  let empty_path = prefix_for_examples + "empty.txt";
  let exit_codes_path = prefix_for_examples + "exit.txt";
  let code = document.getElementById("main");
  let exit_codes = document.getElementById("secondary");
  let select = document.getElementById("select");
  let lang = document.getElementById("lang");
  let cat = document.getElementById("cat");
  let game = document.getElementById("game");

  let ts = new TomSelect(select, {
    create: false,
    maxOptions: null,
    placeholder: "----------",
    wrapperClass: "ts-wrapper right grow",
    sortField: { field: "$order", direction: "asc" },
    score: function (search) {
      const term = search.toLowerCase().trim();
      return function (item) {
        return FuzzySearch.tomSelectScore(term, item.text);
      };
    },
  });

  let lock = false;
  let examples = null;
  let last_selected_example = "";

  function refreshHighlights() {
    if (typeof Highlight !== "undefined") {
      Highlight.refresh("main");
      Highlight.refresh("secondary");
      Highlight.refresh("output");
    }
  }

  function getFile(url, success_callback, callback) {
    lock = true;
    let xhr = new XMLHttpRequest();
    xhr.open("GET", url);
    // xhr.setRequestHeader("Cache-Control", "no-cache");
    //xhr.setRequestHeader("Cache-Control", "max-age=0");
    xhr.overrideMimeType("text/plain");
    xhr.addEventListener("readystatechange", () => {
      if (xhr.readyState == 4) {
        if (xhr.status == 200) {
          success_callback(xhr.responseText);
        } else {
          console.log("Unknown file " + url);
        }
        lock = false;
        if (callback) callback();
      }
    });
    xhr.send();
  }

  function fileToField(url, field, callback) {
    getFile(
      url,
      (content) => {
        let langr = lang.value;
        let langnr = langr.substring(0, 3);
        let gamev = game.value;
        let str = content.replaceAll("{LANG}", langr.toUpperCase());
        str = str.replaceAll("{lang}", langr.toLowerCase());
        str = str.replaceAll("{LANGNR}", langnr.toUpperCase());
        str = str.replaceAll("{langnr}", langnr.toLowerCase());
        str = str.replaceAll("{GAME}", gamev.toUpperCase());
        str = str.replaceAll("{game}", gamev.toLowerCase());
        field.value = str;
        refreshHighlights();
      },
      callback,
    );
  }

  function updateCode() {
    if (examples == null) return;
    if (!lock) {
      if (select.value) {
        let obj = examples[parseInt(select.value)];
        fileToField(prefix_for_examples + obj[lang.value], code, null);
      } else {
        fileToField(empty_path, code, null);
      }
    }
  }

  function updateSelectField() {
    if (examples == null) return;

    let language = lang.value;
    let gam = game.value;
    let category = cat.value;
    let options = [];
    let newSelectedValue = "";
    examples.forEach((element, index) => {
      if (
        language in element &&
        (category == "" || ("cat" in element && element["cat"].includes(category))) &&
        (gam == "" || ("game" in element && element["game"].includes(gam)))
      ) {
        let val = index.toString();
        if (val == last_selected_example) newSelectedValue = val;
        options.push({ value: val, text: element["name"] });
      }
    });

    ts.clear(true);
    ts.clearOptions();
    options.forEach((o) => ts.addOption(o));
    ts.setValue(newSelectedValue, true);

    updateCode();
  }

  function loadExamples(callback) {
    getFile(
      list_path,
      (content) => {
        examples = JSON.parse(content);
        updateSelectField();
      },
      callback,
    );
  }

  function langChanged() {
    url.searchParams.set("lang", lang.value);
    window.history.replaceState(null, null, url);
    updateSelectField();
  }
  function gameChanged() {
    url.searchParams.set("game", game.value);
    window.history.replaceState(null, null, url);
    updateSelectField();
  }

  Options.resolveLangGame(url);

  let langval = url.searchParams.get("lang");
  if (langval) lang.value = langval;

  let gameval = url.searchParams.get("game");
  if (gameval) game.value = gameval;

  window.onDefaultsChanged = () => {
    updateSelectField();
  };

  ts.on("change", (value) => {
    last_selected_example = value;
    updateCode();
  });

  lang.addEventListener("change", langChanged);
  game.addEventListener("change", gameChanged);
  cat.addEventListener("change", updateSelectField);

  fileToField(exit_codes_path, exit_codes, () => {
    fileToField(empty_path, code, () => {
      loadExamples(null);
    });
  });
});

/* ===== TABS ===== */

function openTab(_, id) {
  let active = document.getElementsByClassName("tabcontent-active")[0];
  active.className = active.className.replace("tabcontent-active", "tabcontent");
  let target = document.getElementById(id);
  target.className = target.className.replace("tabcontent", "tabcontent-active");
}

/* ===== COMPUTE ===== */

const ACE_JS_SRC = "ace_js.bc.js?2";
const ACE_WORKER_SRC = "ace_worker.js";

let aceWorker = null;
let aceWorkerReady = false;
let aceWorkerFailed = false;
let aceRequestId = 0;
let aceQueued = null;
let aceComputing = false;
let aceMainScriptLoading = null;

function aceButtons() {
  return {
    compute: document.getElementById("compute"),
    computeNext: document.getElementById("computeNext"),
  };
}

function setComputing(busy) {
  aceComputing = busy;
  document.body.classList.toggle("computing", busy);
  const { compute, computeNext } = aceButtons();
  if (compute) {
    if (!compute.dataset.label) {
      compute.dataset.label = compute.textContent.trim();
    }
    compute.disabled = false;
    compute.textContent = busy ? "Cancel" : compute.dataset.label;
    compute.title = busy ? "Cancel the current computation" : "";
  }
  if (computeNext) computeNext.disabled = busy;
}

function applyComputeResult(txt) {
  const output = document.getElementById("output");
  output.value = txt;
  if (typeof Highlight !== "undefined") Highlight.refresh("output");
}

function finishCompute(data) {
  if (!data || data.id !== aceRequestId) return;
  aceQueued = null;
  setComputing(false);
  if (data.ok) {
    applyComputeResult(data.txt);
  } else {
    applyComputeResult(data.error || "Computation failed.");
  }
}

function runOnMainThread(msg) {
  const { compute } = aceButtons();
  if (compute) {
    compute.disabled = true;
    compute.textContent = compute.dataset.label || "Compute";
    compute.title = "";
  }
  const run = () => {
    try {
      if (typeof aceGen === "undefined") {
        throw new Error("Generator failed to load");
      }
      const result = msg.next
        ? aceGen.buildNext(msg.lang, msg.game, msg.code, msg.exitCodes)
        : aceGen.build(msg.lang, msg.game, msg.code, msg.exitCodes);
      const txt = result && result[1] != null ? String(result[1]) : "";
      finishCompute({ id: msg.id, ok: true, txt });
    } catch (error) {
      finishCompute({
        id: msg.id,
        ok: false,
        error: error && error.message ? error.message : String(error),
      });
    }
  };
  window.setTimeout(run, 20);
}

function loadAceGenOnMainThread() {
  if (typeof aceGen !== "undefined") return Promise.resolve();
  if (aceMainScriptLoading) return aceMainScriptLoading;
  aceMainScriptLoading = new Promise((resolve, reject) => {
    const script = document.createElement("script");
    script.src = ACE_JS_SRC;
    script.onload = () => resolve();
    script.onerror = () =>
      reject(new Error("Failed to load " + ACE_JS_SRC.split("?")[0]));
    document.head.appendChild(script);
  });
  return aceMainScriptLoading;
}

function stopWorker() {
  if (!aceWorker) return;
  aceWorker.onerror = null;
  aceWorker.onmessage = null;
  aceWorker.terminate();
  aceWorker = null;
  aceWorkerReady = false;
}

function fallbackToMainThread(msg) {
  aceWorkerFailed = true;
  stopWorker();
  loadAceGenOnMainThread()
    .then(() => runOnMainThread(msg))
    .catch((error) => {
      finishCompute({
        id: msg.id,
        ok: false,
        error: error && error.message ? error.message : String(error),
      });
    });
}

function startWorker() {
  if (aceWorker || aceWorkerFailed || typeof Worker === "undefined") {
    if (typeof Worker === "undefined") aceWorkerFailed = true;
    return;
  }
  try {
    aceWorker = new Worker(ACE_WORKER_SRC);
  } catch (_error) {
    aceWorkerFailed = true;
    return;
  }
  aceWorker.onmessage = (event) => {
    const data = event.data;
    if (data && data.type === "ready") {
      aceWorkerReady = true;
      if (aceQueued) {
        const queued = aceQueued;
        aceQueued = null;
        aceWorker.postMessage(queued);
      }
      return;
    }
    finishCompute(data);
  };
  aceWorker.onerror = () => {
    if (!aceWorkerReady) {
      const queued = aceQueued;
      aceQueued = null;
      if (queued) fallbackToMainThread(queued);
      else {
        aceWorkerFailed = true;
        stopWorker();
      }
      return;
    }
    if (aceComputing) {
      finishCompute({
        id: aceRequestId,
        ok: false,
        error: "The generator worker crashed.",
      });
    }
    restartWorker();
  };
}

function restartWorker() {
  stopWorker();
  aceQueued = null;
  if (!aceWorkerFailed) startWorker();
}

function cancelCompute() {
  aceRequestId += 1;
  aceQueued = null;
  setComputing(false);
  restartWorker();
}

function runCompute(next) {
  const lang = document.getElementById("lang");
  const game = document.getElementById("game");
  const main = document.getElementById("main");
  const secondary = document.getElementById("secondary");
  aceRequestId += 1;
  const msg = {
    id: aceRequestId,
    next,
    lang: lang.value,
    game: game.value,
    code: main.value,
    exitCodes: secondary.value,
  };
  setComputing(true);
  if (aceWorkerFailed) {
    loadAceGenOnMainThread()
      .then(() => runOnMainThread(msg))
      .catch((error) => {
        finishCompute({
          id: msg.id,
          ok: false,
          error: error && error.message ? error.message : String(error),
        });
      });
    return;
  }
  startWorker();
  if (!aceWorker) {
    fallbackToMainThread(msg);
    return;
  }
  if (aceWorkerReady) aceWorker.postMessage(msg);
  else aceQueued = msg;
}

function compute() {
  if (aceComputing) {
    cancelCompute();
    return;
  }
  runCompute(false);
}

function computeNext() {
  if (aceComputing) return;
  runCompute(true);
}

startWorker();
