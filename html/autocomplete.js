// Credits to https://phuoc.ng/collection/mirror-a-text-area/add-autocomplete-to-your-text-area/

document.addEventListener("DOMContentLoaded", () => {
  const containerEle = document.getElementById("container");
  const textarea = document.getElementById("main");
  if (!containerEle || !textarea) return;

  const suggestionsEle = document.createElement("div");
  suggestionsEle.classList.add("container__suggestions");
  containerEle.appendChild(suggestionsEle);

  let activeWordStart = 0;
  let activeWordEnd = 0;
  let currentSuggestionIndex = -1;
  let suppressAutocomplete = false;

  const clearFocusedSuggestions = () => {
    suggestionsEle.querySelectorAll(".container__suggestion--focused").forEach((el) => {
      el.classList.remove("container__suggestion--focused");
    });
  };

  const getHighlightLayer = () => {
    const editor = typeof Highlight !== "undefined" ? Highlight.getEditor("main") : null;
    return editor ? editor.layer : null;
  };

  const hideSuggestions = () => {
    suggestionsEle.style.display = "none";
    currentSuggestionIndex = -1;
  };

  const findIndexesOfCurrentWord = () => {
    const currentValue = textarea.value;
    const cursorPos = textarea.selectionStart;

    let startIndex = cursorPos - 1;
    while (startIndex >= 0 && !/\s/.test(currentValue[startIndex])) {
      startIndex--;
    }
    let endIndex = cursorPos;
    while (endIndex < currentValue.length && !/\s/.test(currentValue[endIndex])) {
      endIndex++;
    }

    return [startIndex + 1, endIndex];
  };

  const replaceCurrentWord = (newWord) => {
    suppressAutocomplete = true;
    const currentValue = textarea.value;
    const startIndex = activeWordStart;
    const endIndex = activeWordEnd;

    const newValue = currentValue.substring(0, startIndex) + newWord + currentValue.substring(endIndex);
    const scroll = textarea.scrollTop;
    textarea.value = newValue;
    textarea.focus();
    textarea.selectionStart = textarea.selectionEnd = startIndex + newWord.length;
    textarea.scrollTop = scroll;
    hideSuggestions();
    if (typeof Highlight !== "undefined") Highlight.refresh("main");
    textarea.dispatchEvent(new Event("input", { bubbles: true }));
  };

  const positionSuggestions = (startIndex, endIndex) => {
    const currentValue = textarea.value;
    const currentWord = currentValue.substring(startIndex, endIndex);
    const textBeforeWord = currentValue.substring(0, startIndex);
    const textAfterWord = currentValue.substring(endIndex);

    const layer = getHighlightLayer();
    if (!layer) return null;

    const mirror = document.createElement("div");
    mirror.className = "autocomplete-mirror";
    mirror.style.position = "absolute";
    mirror.style.top = "0";
    mirror.style.left = "0";
    mirror.style.visibility = "hidden";
    mirror.style.pointerEvents = "none";
    mirror.style.whiteSpace = "pre-wrap";
    mirror.style.wordWrap = "break-word";

    const textareaStyles = window.getComputedStyle(textarea);
    [
      "boxSizing",
      "borderTopWidth",
      "borderRightWidth",
      "borderBottomWidth",
      "borderLeftWidth",
      "fontFamily",
      "fontSize",
      "fontWeight",
      "letterSpacing",
      "lineHeight",
      "paddingTop",
      "paddingRight",
      "paddingBottom",
      "paddingLeft",
      "tabSize",
      "width",
      "height",
    ].forEach((prop) => {
      mirror.style[prop] = textareaStyles[prop];
    });

    const pre = document.createTextNode(textBeforeWord);
    const post = document.createTextNode(textAfterWord + (textAfterWord.endsWith("\n") ? " " : ""));
    const caretEle = document.createElement("span");
    caretEle.className = "autocomplete-caret";
    caretEle.append(document.createTextNode(currentWord));

    mirror.append(pre, caretEle, post);
    containerEle.appendChild(mirror);
    mirror.scrollTop = textarea.scrollTop;

    const rect = caretEle.getBoundingClientRect();
    mirror.remove();

    return rect;
  };

  const showSuggestions = (matches, startIndex, endIndex) => {
    activeWordStart = startIndex;
    activeWordEnd = endIndex;
    currentSuggestionIndex = -1;

    const rect = positionSuggestions(startIndex, endIndex);
    if (!rect) {
      hideSuggestions();
      return;
    }

    suggestionsEle.style.top = `${rect.top + rect.height}px`;
    suggestionsEle.style.left = `${rect.left}px`;

    suggestionsEle.innerHTML = "";
    suggestionsEle.scrollTop = 0;
    matches.forEach((match, index) => {
      const option = document.createElement("div");
      option.innerText = match;
      option.classList.add("container__suggestion");
      option.addEventListener("mouseenter", () => {
        clearFocusedSuggestions();
        option.classList.add("container__suggestion--focused");
        currentSuggestionIndex = index;
      });
      option.addEventListener("mouseleave", () => {
        option.classList.remove("container__suggestion--focused");
        currentSuggestionIndex = -1;
      });
      option.addEventListener("mousedown", (e) => {
        e.preventDefault();
        e.stopPropagation();
        replaceCurrentWord(pkmn_data_map[match].toString());
      });
      suggestionsEle.appendChild(option);
    });
    suggestionsEle.style.display = "block";
  };

  const updateSuggestions = () => {
    if (suppressAutocomplete) {
      hideSuggestions();
      return;
    }

    const currentValue = textarea.value;
    const [startIndex, endIndex] = findIndexesOfCurrentWord();
    if (endIndex <= startIndex) {
      hideSuggestions();
      return;
    }

    const remaining = currentValue.substring(endIndex);
    const lineBreak = remaining.indexOf("\n");
    const lineEnd = lineBreak >= 0 ? remaining.substring(0, lineBreak) : remaining;
    const tags = lineEnd.match(/@input:\w*/g);
    if (tags === null) {
      hideSuggestions();
      return;
    }
    const groups = tags.map((x) => x.substring(7));
    const suggestions = groups
      .map((x) => pkmn_data[x])
      .filter((x) => x !== undefined)
      .flat(1);

    const currentWord = currentValue.substring(startIndex, endIndex);
    if (currentWord === "") {
      hideSuggestions();
      return;
    }

    const matches = FuzzySearch.filterAndRank(
      suggestions,
      currentWord,
      (suggestion) => suggestion,
      (suggestion) => pkmn_data_map[suggestion],
    );
    if (matches.length === 0) {
      hideSuggestions();
      return;
    }

    showSuggestions(matches, startIndex, endIndex);
  };

  ["input", "selectionchange"].forEach((e) => textarea.addEventListener(e, updateSuggestions));

  textarea.addEventListener("keydown", () => {
    suppressAutocomplete = false;
  });

  const scrollSuggestionIntoView = (option) => {
    const viewTop = suggestionsEle.scrollTop;
    const viewBottom = viewTop + suggestionsEle.clientHeight;
    const optionTop = option.offsetTop;
    const optionBottom = optionTop + option.offsetHeight;

    if (optionTop < viewTop) {
      suggestionsEle.scrollTop = optionTop;
    } else if (optionBottom > viewBottom) {
      suggestionsEle.scrollTop = optionBottom - suggestionsEle.clientHeight;
    }
  };

  const focusSuggestionAt = (index) => {
    const suggestions = suggestionsEle.querySelectorAll(".container__suggestion");
    if (suggestions.length === 0) return;

    clearFocusedSuggestions();
    currentSuggestionIndex = index;
    const option = suggestions[currentSuggestionIndex];
    option.classList.add("container__suggestion--focused");
    scrollSuggestionIntoView(option);
  };

  textarea.addEventListener("keydown", (e) => {
    if (!["Enter", "Escape", "Tab"].includes(e.key)) {
      return;
    }

    const suggestions = suggestionsEle.querySelectorAll(".container__suggestion");
    const numSuggestions = suggestions.length;
    if (numSuggestions === 0 || suggestionsEle.style.display === "none") {
      return;
    }
    e.preventDefault();
    switch (e.key) {
      case "Tab":
        if (e.shiftKey) {
          focusSuggestionAt(currentSuggestionIndex <= 0 ? numSuggestions - 1 : currentSuggestionIndex - 1);
        } else {
          focusSuggestionAt((currentSuggestionIndex + 1) % numSuggestions);
        }
        break;
      case "Enter":
        const pickIndex = currentSuggestionIndex >= 0 ? currentSuggestionIndex : 0;
        replaceCurrentWord(pkmn_data_map[suggestions[pickIndex].innerText].toString());
        break;
      case "Escape":
        hideSuggestions();
        break;
      default:
        break;
    }
  });
});
