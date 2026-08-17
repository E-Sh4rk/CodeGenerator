const Compute = (() => {
  const LABELS = {
    compute: "Compute",
    computeNext: "Compute another",
  };

  let worker = null;
  let blobUrl = null;
  let jobId = 0;
  let inProgress = false;
  let sourceId = null;

  // Runs inside the worker when the page is opened as a file:// URL.
  function onWorkerMessage(e) {
    var d = e.data;
    try {
      var fn = d.op === "buildNext" ? aceGen.buildNext : aceGen.build;
      var result = fn.call(aceGen, d.lang, d.game, d.code, d.exitCodes);
      self.postMessage({ id: d.id, ok: true, txt: result[1] });
    } catch (err) {
      self.postMessage({ id: d.id, ok: false, error: String(err) });
    }
  }

  function setBlocked(btn, blocked) {
    btn.classList.toggle("blocked", blocked);
    btn.setAttribute("aria-disabled", blocked ? "true" : "false");
  }

  function setBusy(busy, buttonId) {
    inProgress = busy;
    let computeBtn = document.getElementById("compute");
    let nextBtn = document.getElementById("computeNext");
    if (!busy) {
      sourceId = null;
      computeBtn.textContent = LABELS.compute;
      nextBtn.textContent = LABELS.computeNext;
      setBlocked(computeBtn, false);
      setBlocked(nextBtn, false);
      return;
    }
    sourceId = buttonId;
    computeBtn.textContent = buttonId === "compute" ? "Cancel" : LABELS.compute;
    nextBtn.textContent = buttonId === "computeNext" ? "Cancel" : LABELS.computeNext;
    setBlocked(computeBtn, buttonId !== "compute");
    setBlocked(nextBtn, buttonId !== "computeNext");
  }

  function applyResult(txt) {
    document.getElementById("output").value = txt;
    if (typeof Highlight !== "undefined") Highlight.refresh("output");
  }

  function terminate() {
    if (worker) {
      worker.terminate();
      worker = null;
    }
    if (blobUrl) {
      URL.revokeObjectURL(blobUrl);
      blobUrl = null;
    }
  }

  function spawn() {
    terminate();
    try {
      worker = new Worker("compute_worker.js");
    } catch (_) {
      let engine = JSON.stringify(new URL("ace_js.bc.js?2", document.baseURI).href);
      blobUrl = URL.createObjectURL(
        new Blob(["importScripts(" + engine + ");\nself.onmessage = " + onWorkerMessage], {
          type: "text/javascript",
        }),
      );
      worker = new Worker(blobUrl);
    }
    worker.onmessage = onMessage;
    worker.onerror = onError;
  }

  function onMessage(e) {
    let data = e.data || {};
    if (data.id !== jobId) return;
    setBusy(false);
    applyResult(data.ok ? data.txt : data.error || "Computation failed.");
  }

  function onError(e) {
    terminate();
    if (!inProgress) return;
    jobId += 1;
    setBusy(false);
    applyResult(e.message || "Computation worker failed.");
  }

  function start(op, buttonId) {
    if (inProgress) return;
    jobId += 1;
    setBusy(true, buttonId);
    if (op === "build" || !worker) spawn();
    worker.postMessage({
      id: jobId,
      op,
      lang: document.getElementById("lang").value,
      game: document.getElementById("game").value,
      code: document.getElementById("main").value,
      exitCodes: document.getElementById("secondary").value,
    });
  }

  function cancel() {
    if (!inProgress) return;
    jobId += 1;
    terminate();
    setBusy(false);
  }

  function onClick(op, buttonId) {
    if (inProgress && sourceId === buttonId) cancel();
    else if (!inProgress) start(op, buttonId);
  }

  return {
    compute() {
      onClick("build", "compute");
    },
    computeNext() {
      onClick("buildNext", "computeNext");
    },
  };
})();

function compute() {
  Compute.compute();
}

function computeNext() {
  Compute.computeNext();
}
