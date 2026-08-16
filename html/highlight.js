const Highlight = (() => {
  const editors = new Map();

  const MIRROR_PROPS = [
    "boxSizing",
    "borderTopWidth",
    "borderRightWidth",
    "borderBottomWidth",
    "borderLeftWidth",
    "borderStyle",
    "borderRadius",
    "fontFamily",
    "fontSize",
    "fontWeight",
    "fontStyle",
    "letterSpacing",
    "textSizeAdjust",
    "lineHeight",
    "paddingTop",
    "paddingRight",
    "paddingBottom",
    "paddingLeft",
    "textDecoration",
    "textIndent",
    "textTransform",
    "wordSpacing",
    "tabSize",
  ];

  function escapeHtml(text) {
    return text
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;");
  }

  function charClass(ch) {
    if (/[0-9]/.test(ch)) return "number";
    if (/[A-Za-z]/.test(ch) || /[\u3040-\u30ff\u4e00-\u9fff]/.test(ch))
      return "letter";
    if (/\s/.test(ch)) return "space";
    return "symbol";
  }

  function wrapChar(ch, cls, inBracket) {
    if (ch === " " || ch === "\t") return ch;
    const settings = Options.load();
    const box = settings.boxNames;
    let type = cls;
    if (type === "auto") type = charClass(ch);
    let styleClass = "hl-box-" + type;
    if (inBracket && type !== "space")
      styleClass = "hl-box-bracket-inner hl-box-" + type;
    let weight = "";
    if (type === "number" && box.numberBold) weight = " hl-box-bold";
    if (type === "symbol" && box.symbolBold) weight = " hl-box-bold";
    return `<span class="${styleClass}${weight}">${escapeHtml(ch)}</span>`;
  }

  function highlightBoxNameLine(line) {
    const match = line.match(/^(Box\s+)(\d+)(:\s+)(.+?)(\s+\[)(.+)(\])$/);
    if (!match) return escapeHtml(line);

    const settings = Options.load();
    if (!settings.boxNames.enabled) return escapeHtml(line);

    let html = `<span class="hl-box-label">${escapeHtml(match[1])}</span>`;
    html += `<span class="hl-box-label">${escapeHtml(match[2])}</span>`;
    html += `<span class="hl-box-label">${escapeHtml(match[3])}</span>`;

    for (const ch of match[4]) {
      html += wrapChar(ch, "auto", false);
    }

    html += `<span class="hl-box-bracket">${escapeHtml(match[5])}</span>`;
    for (const ch of match[6]) {
      html += wrapChar(ch, "auto", true);
    }
    html += `<span class="hl-box-bracket">${escapeHtml(match[7])}</span>`;
    return html;
  }

  function highlightCodeLine(line) {
    const settings = Options.load();
    if (!settings.comments.enabled) return escapeHtml(line);

    const trimmed = line.trimStart();
    const leading = line.slice(0, line.length - trimmed.length);

    if (trimmed.startsWith("@@")) {
      return (
        escapeHtml(leading) +
        `<span class="hl-comment-header">${escapeHtml(trimmed)}</span>`
      );
    }
    if (trimmed.startsWith("//")) {
      return (
        escapeHtml(leading) +
        `<span class="hl-comment-slash">${escapeHtml(trimmed)}</span>`
      );
    }
    if (trimmed.startsWith(";")) {
      return (
        escapeHtml(leading) +
        `<span class="hl-comment-semicolon">${escapeHtml(trimmed)}</span>`
      );
    }
    if (trimmed.startsWith("%%")) {
      return (
        escapeHtml(leading) +
        `<span class="hl-comment-percent">${escapeHtml(trimmed)}</span>`
      );
    }

    const commentIdx = line.indexOf(";");
    if (commentIdx >= 0 && !line.trimStart().startsWith("0x")) {
      const code = line.slice(0, commentIdx);
      const comment = line.slice(commentIdx);
      if (
        comment.match(/^;\s*\((altered|filler)\)/) ||
        comment.includes("EXIT CODE")
      ) {
        return (
          escapeHtml(code) +
          `<span class="hl-comment-semicolon">${escapeHtml(comment)}</span>`
        );
      }
    }

    return escapeHtml(line);
  }

  function highlightText(text, mode) {
    const lines = text.split("\n");
    return lines
      .map((line) => {
        if (mode === "output") {
          if (/^Box\s+\d+:/.test(line)) return highlightBoxNameLine(line);
          return highlightCodeLine(line);
        }
        return highlightCodeLine(line);
      })
      .join("\n");
  }

  function copyTextareaStyles(textarea, layer) {
    const styles = window.getComputedStyle(textarea);
    MIRROR_PROPS.forEach((prop) => {
      layer.style[prop] = styles[prop];
    });
    layer.style.borderColor = "transparent";
    layer.style.overflow = "hidden";
  }

  function createEditor(textarea, mode) {
    const wrap = textarea.closest(".editor-wrap, .container");
    if (!wrap) return null;

    wrap.classList.add("editor-wrap");

    let layer = wrap.querySelector(".highlight-layer");
    if (!layer) {
      layer = document.createElement("div");
      layer.className = "highlight-layer";
      layer.setAttribute("aria-hidden", "true");
      wrap.insertBefore(layer, textarea);
    }

    const syncStyles = () => copyTextareaStyles(textarea, layer);

    const update = () => {
      syncStyles();
      const settings = Options.load();
      const commentsOn = settings.comments.enabled;
      const boxOn = settings.boxNames.enabled && mode === "output";
      const active = mode === "output" ? commentsOn || boxOn : commentsOn;

      textarea.classList.toggle("editor--highlighted", active);
      if (!active) {
        layer.innerHTML = "";
        return;
      }

      const value = textarea.value;
      let html = highlightText(value, mode);
      if (value.endsWith("\n")) html += "\n";
      layer.innerHTML = html;
      layer.scrollTop = textarea.scrollTop;
      layer.scrollLeft = textarea.scrollLeft;
    };

    const ro = new ResizeObserver(() => {
      syncStyles();
      update();
    });
    ro.observe(wrap);
    ro.observe(textarea);
    syncStyles();

    textarea.addEventListener("input", update);
    textarea.addEventListener("scroll", () => {
      layer.scrollTop = textarea.scrollTop;
      layer.scrollLeft = textarea.scrollLeft;
    });

    const editor = { textarea, layer, mode, update, syncStyles };
    editors.set(textarea.id, editor);
    update();
    requestAnimationFrame(update);
    return editor;
  }

  function getEditor(textareaId) {
    return editors.get(textareaId) || null;
  }

  function refreshAll() {
    for (const editor of editors.values()) {
      editor.update();
    }
  }

  function refresh(textareaId) {
    const editor = editors.get(textareaId);
    if (editor) editor.update();
  }

  function init() {
    const main = document.getElementById("main");
    const secondary = document.getElementById("secondary");
    const output = document.getElementById("output");
    if (main) createEditor(main, "code");
    if (secondary) createEditor(secondary, "code");
    if (output) createEditor(output, "output");
  }

  window.addEventListener("DOMContentLoaded", init);

  return {
    createEditor,
    getEditor,
    refreshAll,
    refresh,
    highlightText,
    escapeHtml,
    copyTextareaStyles,
  };
})();
