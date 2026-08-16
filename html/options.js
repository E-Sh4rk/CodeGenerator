const Options = (() => {
  const STORAGE_KEY = "acegen-options";

  const COLOR_PRESETS = {
    light: {
      comments: {
        semicolon: "#1a7f37",
        slash: "#1a7f37",
        percent: "#0550ae",
        header: "#8250df",
      },
      boxNames: {
        number: "#0e7490",
        symbol: "#cf222e",
        letter: "#6639ba",
        bracket: "#bc4c00",
        label: "#57606a",
      },
    },
    dark: {
      comments: {
        semicolon: "#7ee787",
        slash: "#7ee787",
        percent: "#79c0ff",
        header: "#d2a8ff",
      },
      boxNames: {
        number: "#56d4dd",
        symbol: "#ff8a80",
        letter: "#e8b4f8",
        bracket: "#ffa657",
        label: "#8b949e",
      },
    },
  };

  const COMMENT_COLOR_FIELDS = [
    { key: "semicolon", label: "; comments" },
    { key: "slash", label: "// comments" },
    { key: "percent", label: "%% annotations" },
    { key: "header", label: "@@ headers" },
  ];

  const BOX_COLOR_FIELDS = [
    { key: "label", label: "Box label" },
    { key: "number", label: "Numbers" },
    { key: "symbol", label: "Symbols" },
    { key: "letter", label: "Letters" },
    { key: "bracket", label: "Brackets" },
  ];

  const DEFAULTS = {
    theme: "system",
    defaults: {
      emerald: { lang: "eng", game: "" },
      frlg: { lang: "eng1", game: "fr" },
      rs: { lang: "eng2", game: "ruby" },
    },
    comments: { enabled: true },
    boxNames: {
      enabled: true,
      numberBold: false,
      symbolBold: false,
    },
    colors: {
      light: deepClone(COLOR_PRESETS.light),
      dark: deepClone(COLOR_PRESETS.dark),
    },
  };

  const PAGE_IDS = {
    files: "emerald",
    files_frlg: "frlg",
    files_rs: "rs",
  };

  let settings = null;
  let colorFieldsBuilt = false;

  function deepClone(obj) {
    return JSON.parse(JSON.stringify(obj));
  }

  function deepMerge(base, override) {
    const result = { ...base };
    for (const key of Object.keys(override)) {
      if (
        override[key] &&
        typeof override[key] === "object" &&
        !Array.isArray(override[key])
      ) {
        result[key] = deepMerge(base[key] || {}, override[key]);
      } else if (override[key] !== undefined) {
        result[key] = override[key];
      }
    }
    return result;
  }

  function migrateStored(stored) {
    const merged = deepMerge(DEFAULTS, stored);

    if (!stored.colors) {
      merged.colors = deepClone(DEFAULTS.colors);

      if (stored.comments) {
        merged.comments.enabled = stored.comments.enabled ?? true;
        const commentColors = pickColorKeys(
          stored.comments,
          COMMENT_COLOR_FIELDS,
        );
        if (Object.keys(commentColors).length > 0) {
          merged.colors.dark.comments = {
            ...merged.colors.dark.comments,
            ...commentColors,
          };
        }
      }
      if (stored.boxNames) {
        merged.boxNames.enabled = stored.boxNames.enabled ?? true;
        merged.boxNames.numberBold = stored.boxNames.numberBold ?? false;
        merged.boxNames.symbolBold = stored.boxNames.symbolBold ?? false;
        const boxColors = pickColorKeys(stored.boxNames, BOX_COLOR_FIELDS);
        if (Object.keys(boxColors).length > 0) {
          merged.colors.dark.boxNames = {
            ...merged.colors.dark.boxNames,
            ...boxColors,
          };
        }
      }
    }

    merged.comments = { enabled: merged.comments?.enabled ?? true };
    merged.boxNames = {
      enabled: merged.boxNames?.enabled ?? true,
      numberBold: merged.boxNames?.numberBold ?? false,
      symbolBold: merged.boxNames?.symbolBold ?? false,
    };

    return merged;
  }

  function pickColorKeys(source, fields) {
    const out = {};
    for (const field of fields) {
      if (source[field.key]) out[field.key] = source[field.key];
    }
    return out;
  }

  function load() {
    if (settings) return settings;
    let stored = {};
    try {
      const raw = localStorage.getItem(STORAGE_KEY);
      if (raw) stored = JSON.parse(raw);
    } catch (_) {
      stored = {};
    }
    const legacyTheme = localStorage.getItem("theme");
    if (!stored.theme && (legacyTheme === "light" || legacyTheme === "dark")) {
      stored.theme = legacyTheme;
    }
    settings = migrateStored(stored);
    return settings;
  }

  function save() {
    localStorage.setItem(STORAGE_KEY, JSON.stringify(settings));
    localStorage.removeItem("theme");
  }

  function getPageId() {
    const ws = document.getElementById("script_workspace");
    if (!ws) return "emerald";
    return PAGE_IDS[ws.value] || "emerald";
  }

  function getResolvedColorMode() {
    const s = load();
    if (s.theme === "dark") return "dark";
    if (s.theme === "light") return "light";
    return window.matchMedia("(prefers-color-scheme: dark)").matches
      ? "dark"
      : "light";
  }

  function applyTheme() {
    const s = load();
    if (s.theme === "system") {
      document.documentElement.removeAttribute("data-theme");
    } else {
      document.documentElement.setAttribute("data-theme", s.theme);
    }
  }

  function applyBodyClasses() {
    if (!document.body) return;
    const s = load();
    document.body.classList.toggle(
      "comments-highlight-off",
      !s.comments.enabled,
    );
    document.body.classList.toggle(
      "boxnames-highlight-off",
      !s.boxNames.enabled,
    );
  }

  function applyCssVariables() {
    const s = load();
    const mode = getResolvedColorMode();
    const palette = s.colors[mode];
    const root = document.documentElement;

    root.style.setProperty("--comment-semicolon", palette.comments.semicolon);
    root.style.setProperty("--comment-slash", palette.comments.slash);
    root.style.setProperty("--comment-percent", palette.comments.percent);
    root.style.setProperty("--comment-header", palette.comments.header);
    root.style.setProperty("--box-number", palette.boxNames.number);
    root.style.setProperty("--box-symbol", palette.boxNames.symbol);
    root.style.setProperty("--box-letter", palette.boxNames.letter);
    root.style.setProperty("--box-bracket", palette.boxNames.bracket);
    root.style.setProperty("--box-label", palette.boxNames.label);
    root.style.setProperty(
      "--box-number-weight",
      s.boxNames.numberBold ? "bold" : "normal",
    );
    root.style.setProperty(
      "--box-symbol-weight",
      s.boxNames.symbolBold ? "bold" : "normal",
    );
    applyBodyClasses();
  }

  function apply() {
    applyTheme();
    applyCssVariables();
    if (typeof Highlight !== "undefined") Highlight.refreshAll();
  }

  function getDefaultsForPage(pageId) {
    const s = load();
    return s.defaults[pageId] || null;
  }

  function saveDefaultsForPage(pageId, lang, game) {
    const s = load();
    s.defaults[pageId] = { lang, game };
    save();
  }

  function resolveLangGame(url) {
    const pageId = getPageId();
    const langEl = document.getElementById("lang");
    const gameEl = document.getElementById("game");
    if (!langEl) return;

    const langParam = url.searchParams.get("lang");
    const gameParam = url.searchParams.get("game");

    if (!langParam) {
      const saved = getDefaultsForPage(pageId);
      if (
        saved &&
        saved.lang &&
        [...langEl.options].some((o) => o.value === saved.lang)
      ) {
        langEl.value = saved.lang;
      }
    }
    if (!gameParam && gameEl) {
      const saved = getDefaultsForPage(pageId);
      if (
        saved &&
        saved.game !== undefined &&
        [...gameEl.options].some((o) => o.value === saved.game)
      ) {
        gameEl.value = saved.game;
      }
    }
  }

  function populateSelect(selectEl, sourceEl) {
    if (!selectEl || !sourceEl) return;
    selectEl.innerHTML = "";
    for (const opt of sourceEl.options) {
      const o = document.createElement("option");
      o.value = opt.value;
      o.textContent = opt.textContent;
      selectEl.appendChild(o);
    }
  }

  function colorInputId(group, key, mode) {
    return `options-${group}-${key}-${mode}`;
  }

  function buildPaletteGrid(group, fields) {
    const grid = document.createElement("div");
    grid.className = "options-palette-grid";

    const headings = document.createElement("div");
    headings.className = "options-palette-grid__headings";
    headings.innerHTML =
      '<span class="options-palette-grid__label-spacer"></span><span>Light</span><span>Dark</span>';
    grid.appendChild(headings);

    for (const field of fields) {
      const row = document.createElement("div");
      row.className = "options-palette-grid__row";

      const name = document.createElement("span");
      name.className = "options-palette-grid__name";
      name.textContent = field.label;
      row.appendChild(name);

      for (const mode of ["light", "dark"]) {
        const id = colorInputId(group, field.key, mode);
        const input = document.createElement("input");
        input.type = "color";
        input.id = id;
        input.title = `${field.label} (${mode} theme)`;
        input.setAttribute("aria-label", `${field.label}, ${mode} theme`);
        input.dataset.group = group;
        input.dataset.key = field.key;
        input.dataset.mode = mode;
        row.appendChild(input);
      }

      grid.appendChild(row);
    }

    return grid;
  }

  function buildColorPaletteFields() {
    if (colorFieldsBuilt) return;

    const commentContainer = document.getElementById(
      "options-comment-color-fields",
    );
    const boxContainer = document.getElementById("options-box-color-fields");
    if (!commentContainer || !boxContainer) return;

    commentContainer.appendChild(
      buildPaletteGrid("comment", COMMENT_COLOR_FIELDS),
    );
    boxContainer.appendChild(buildPaletteGrid("box", BOX_COLOR_FIELDS));

    const modal = document.getElementById("options-modal");
    modal.querySelectorAll('input[type="color"][data-mode]').forEach((el) => {
      el.addEventListener("change", readFormIntoSettings);
    });

    colorFieldsBuilt = true;
  }

  function syncFormFromSettings() {
    const s = load();
    const modal = document.getElementById("options-modal");
    if (!modal) return;

    buildColorPaletteFields();

    modal.querySelectorAll('input[name="theme"]').forEach((r) => {
      r.checked = r.value === s.theme;
    });

    const langSelect = document.getElementById("options-default-lang");
    const gameSelect = document.getElementById("options-default-game");
    const pageLang = document.getElementById("lang");
    const pageGame = document.getElementById("game");
    populateSelect(langSelect, pageLang);
    populateSelect(gameSelect, pageGame);

    const pageId = getPageId();
    const saved = s.defaults[pageId];
    if (saved) {
      if (
        langSelect &&
        [...langSelect.options].some((o) => o.value === saved.lang)
      ) {
        langSelect.value = saved.lang;
      }
      if (
        gameSelect &&
        saved.game !== undefined &&
        [...gameSelect.options].some((o) => o.value === saved.game)
      ) {
        gameSelect.value = saved.game;
      }
    }

    const setChecked = (id, val) => {
      const el = document.getElementById(id);
      if (el) el.checked = val;
    };

    setChecked("options-comments-enabled", s.comments.enabled);
    setChecked("options-boxnames-enabled", s.boxNames.enabled);
    setChecked("options-box-number-bold", s.boxNames.numberBold);
    setChecked("options-box-symbol-bold", s.boxNames.symbolBold);

    for (const mode of ["light", "dark"]) {
      for (const field of COMMENT_COLOR_FIELDS) {
        const el = document.getElementById(
          colorInputId("comment", field.key, mode),
        );
        if (el) el.value = s.colors[mode].comments[field.key];
      }
      for (const field of BOX_COLOR_FIELDS) {
        const el = document.getElementById(
          colorInputId("box", field.key, mode),
        );
        if (el) el.value = s.colors[mode].boxNames[field.key];
      }
    }

    const gameRow = document.getElementById("options-default-game-row");
    if (gameRow && pageGame) {
      gameRow.style.display = pageGame.style.display === "none" ? "none" : "";
    }
  }

  function readFormIntoSettings() {
    const s = load();
    const themeRadio = document.querySelector('input[name="theme"]:checked');
    if (themeRadio) s.theme = themeRadio.value;

    s.comments.enabled = document.getElementById(
      "options-comments-enabled",
    ).checked;
    s.boxNames.enabled = document.getElementById(
      "options-boxnames-enabled",
    ).checked;
    s.boxNames.numberBold = document.getElementById(
      "options-box-number-bold",
    ).checked;
    s.boxNames.symbolBold = document.getElementById(
      "options-box-symbol-bold",
    ).checked;

    for (const mode of ["light", "dark"]) {
      for (const field of COMMENT_COLOR_FIELDS) {
        const el = document.getElementById(
          colorInputId("comment", field.key, mode),
        );
        if (el) s.colors[mode].comments[field.key] = el.value;
      }
      for (const field of BOX_COLOR_FIELDS) {
        const el = document.getElementById(
          colorInputId("box", field.key, mode),
        );
        if (el) s.colors[mode].boxNames[field.key] = el.value;
      }
    }

    save();
    apply();
  }

  function openModal() {
    syncFormFromSettings();
    document
      .getElementById("options-modal")
      .classList.add("options-modal--open");
  }

  function closeModal() {
    document
      .getElementById("options-modal")
      .classList.remove("options-modal--open");
  }

  function initModal() {
    const btn = document.getElementById("options-button");
    if (btn) btn.addEventListener("click", openModal);

    const modal = document.getElementById("options-modal");
    if (!modal) return;

    buildColorPaletteFields();

    modal
      .querySelector(".options-modal__backdrop")
      .addEventListener("click", closeModal);
    modal
      .querySelector(".options-modal__close")
      .addEventListener("click", closeModal);

    modal.querySelectorAll('input[name="theme"]').forEach((el) => {
      el.addEventListener("change", readFormIntoSettings);
    });

    [
      "options-comments-enabled",
      "options-boxnames-enabled",
      "options-box-number-bold",
      "options-box-symbol-bold",
    ].forEach((id) => {
      const el = document.getElementById(id);
      if (el) el.addEventListener("change", readFormIntoSettings);
    });

    document
      .getElementById("options-save-defaults")
      .addEventListener("click", () => {
        const langSelect = document.getElementById("options-default-lang");
        const gameSelect = document.getElementById("options-default-game");
        saveDefaultsForPage(
          getPageId(),
          langSelect.value,
          gameSelect ? gameSelect.value : "",
        );
        const pageLang = document.getElementById("lang");
        const pageGame = document.getElementById("game");
        if (pageLang) pageLang.value = langSelect.value;
        if (pageGame && gameSelect) pageGame.value = gameSelect.value;
        if (typeof window.onDefaultsChanged === "function")
          window.onDefaultsChanged();
      });

    document
      .getElementById("options-use-current")
      .addEventListener("click", () => {
        const pageLang = document.getElementById("lang");
        const pageGame = document.getElementById("game");
        const langSelect = document.getElementById("options-default-lang");
        const gameSelect = document.getElementById("options-default-game");
        if (pageLang && langSelect) langSelect.value = pageLang.value;
        if (pageGame && gameSelect) gameSelect.value = pageGame.value;
      });
  }

  load();
  applyTheme();
  applyCssVariables();

  window
    .matchMedia("(prefers-color-scheme: dark)")
    .addEventListener("change", () => {
      if (load().theme === "system") apply();
    });

  window.addEventListener("DOMContentLoaded", () => {
    applyBodyClasses();
    initModal();
  });

  return {
    load,
    save,
    apply,
    getPageId,
    getResolvedColorMode,
    getDefaultsForPage,
    saveDefaultsForPage,
    resolveLangGame,
    openModal,
    closeModal,
  };
})();
