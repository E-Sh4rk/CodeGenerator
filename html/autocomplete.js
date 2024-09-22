// Credits to https://phuoc.ng/collection/mirror-a-text-area/add-autocomplete-to-your-text-area/

document.addEventListener('DOMContentLoaded', () => {
    const containerEle = document.getElementById('container');
    const textarea = document.getElementById('main');

    const mirroredEle = document.createElement('div');
    mirroredEle.textContent = textarea.value;
    mirroredEle.classList.add('container__mirror');
    containerEle.prepend(mirroredEle);

    const suggestionsEle = document.createElement('div');
    suggestionsEle.classList.add('container__suggestions');
    containerEle.appendChild(suggestionsEle);

    const textareaStyles = window.getComputedStyle(textarea);
    [
        'border',
        'boxSizing',
        'fontFamily',
        'fontSize',
        'fontWeight',
        'letterSpacing',
        'lineHeight',
        'padding',
        'textDecoration',
        'textIndent',
        'textTransform',
        'whiteSpace',
        'wordSpacing',
        'wordWrap',
    ].forEach((property) => {
        mirroredEle.style[property] = textareaStyles[property];
    });
    mirroredEle.style.borderColor = 'transparent';

    const parseValue = (v) => v.endsWith('px') ? parseInt(v.slice(0, -2), 10) : 0;
    const borderWidth = parseValue(textareaStyles.borderWidth);

    const ro = new ResizeObserver(() => {
        mirroredEle.style.width = `${textarea.clientWidth + 2 * borderWidth}px`;
        mirroredEle.style.height = `${textarea.clientHeight + 2 * borderWidth}px`;
    });
    ro.observe(textarea);

    textarea.addEventListener('scroll', () => {
        mirroredEle.scrollTop = textarea.scrollTop;
    });

    const findIndexesOfCurrentWord = () => {
        // Get current value and cursor position
        const currentValue = textarea.value;
        const cursorPos = textarea.selectionStart;

        // Iterate backwards through characters until we find a space or newline character
        let startIndex = cursorPos-1;
        while (startIndex >= 0 && !/\s/.test(currentValue[startIndex])) {
            startIndex--;
        }
        // Iterate forward through characters until we find a space or newline character
        let endIndex = cursorPos;
        while (endIndex < currentValue.length && !/\s/.test(currentValue[endIndex])) {
            endIndex++;
        }
        
        return [startIndex+1, endIndex];
    };

    // Replace current word with selected suggestion
    const replaceCurrentWord = (newWord) => {
        const currentValue = textarea.value;
        const [startIndex, endIndex] = findIndexesOfCurrentWord();

        const newValue = currentValue.substring(0, startIndex) +
                        newWord +
                        currentValue.substring(endIndex);
        textarea.value = newValue;
        textarea.focus();
        textarea.selectionStart = textarea.selectionEnd = startIndex + newWord.length;
    };

    ['input', 'selectionchange'].forEach(e =>
        textarea.addEventListener(e, () => {
            const currentValue = textarea.value;
            const [startIndex, endIndex] = findIndexesOfCurrentWord();
            if (endIndex <= startIndex) {
                suggestionsEle.style.display = 'none';
                return;
            }
    
            const remaining = currentValue.substring(endIndex);
            const lineBreak = remaining.indexOf("\n");
            const lineEnd = lineBreak >= 0 ? remaining.substring(0, lineBreak) : remaining;
            const tags = lineEnd.match(/@input:\w*/g)
            if (tags === null) {
                suggestionsEle.style.display = 'none';
                return;
            }
            const groups = tags.map(x => x.substring(7));
            const suggestions = groups.map((x) => pkmn_data[x]).filter((x) => x !== undefined).flat(1);
    
            // Extract just the current word
            const currentWord = currentValue.substring(startIndex, endIndex);
            if (currentWord === '') {
                suggestionsEle.style.display = 'none';
                return;
            }
    
            const matches = suggestions.filter((suggestion) =>
                suggestion.toLowerCase().startsWith(currentWord.toLowerCase()) || parseInt(currentWord) === pkmn_data_map[suggestion]);
            if (matches.length === 0) {
                suggestionsEle.style.display = 'none';
                return;
            }
    
            const textBeforeWord = currentValue.substring(0, startIndex);
            const textAfterWord = currentValue.substring(endIndex);
    
            const pre = document.createTextNode(textBeforeWord);
            const post = document.createTextNode(textAfterWord + (textAfterWord.endsWith("\n") ? " " : ""));
            const caretEle = document.createElement('span');
            caretEle.innerHTML = '';
            caretEle.append(document.createTextNode(currentWord));
    
            mirroredEle.innerHTML = '';
            mirroredEle.append(pre, caretEle, post);
            mirroredEle.scrollTop = textarea.scrollTop;
    
            const rect = caretEle.getBoundingClientRect();
            suggestionsEle.style.top = `${rect.top + rect.height}px`;
            suggestionsEle.style.left = `${rect.left}px`;
    
            suggestionsEle.innerHTML = '';
            matches.forEach((match) => {
                const option = document.createElement('div');
                option.innerText = match;
                option.classList.add('container__suggestion');
                option.addEventListener('click', function() {
                    replaceCurrentWord(pkmn_data_map[this.innerText].toString());
                    suggestionsEle.style.display = 'none';
                });
                suggestionsEle.appendChild(option);
            });
            suggestionsEle.style.display = 'block';
        })
    );

    const clamp = (min, value, max) => Math.min(Math.max(min, value), max);

    let currentSuggestionIndex = -1;
    textarea.addEventListener('keydown', (e) => {
        if (!['ArrowDown', 'ArrowUp', 'Enter', 'Escape', 'Tab'].includes(e.key)) {
            return;
        }

        const suggestions = suggestionsEle.querySelectorAll('.container__suggestion');
        const numSuggestions = suggestions.length;
        if (numSuggestions === 0 || suggestionsEle.style.display === 'none') {
            return;
        }
        e.preventDefault();
        switch (e.key) {
            case 'ArrowDown':
            case 'Tab':
                suggestions[
                    clamp(0, currentSuggestionIndex, numSuggestions - 1)
                ].classList.remove('container__suggestion--focused');
                currentSuggestionIndex = clamp(0, currentSuggestionIndex + 1, numSuggestions - 1);
                suggestions[currentSuggestionIndex].classList.add('container__suggestion--focused');
                break;
            case 'ArrowUp':
                suggestions[
                    clamp(0, currentSuggestionIndex, numSuggestions - 1)
                ].classList.remove('container__suggestion--focused');
                currentSuggestionIndex = clamp(0, currentSuggestionIndex - 1, numSuggestions - 1);
                suggestions[currentSuggestionIndex].classList.add('container__suggestion--focused');
                break;
            case 'Enter':
                replaceCurrentWord(pkmn_data_map[suggestions[currentSuggestionIndex].innerText].toString());
                currentSuggestionIndex = -1;
                suggestionsEle.style.display = 'none';
                break;
            case 'Escape':
                currentSuggestionIndex = -1;
                suggestionsEle.style.display = 'none';
                break;
            default:
                break;
        }
    });
});