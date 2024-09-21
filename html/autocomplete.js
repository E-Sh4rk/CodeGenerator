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

    const findIndexOfCurrentWord = () => {
        // Get current value and cursor position
        const currentValue = textarea.value;
        const cursorPos = textarea.selectionStart;

        // Iterate backwards through characters until we find a space or newline character
        let startIndex = cursorPos - 1;
        while (startIndex >= 0 && !/\s/.test(currentValue[startIndex])) {
            startIndex--;
        }
        return startIndex;
    };

    // Replace current word with selected suggestion
    const replaceCurrentWord = (newWord) => {
        const currentValue = textarea.value;
        const cursorPos = textarea.selectionStart;
        const startIndex = findIndexOfCurrentWord();

        const newValue = currentValue.substring(0, startIndex + 1) +
                        newWord +
                        currentValue.substring(cursorPos);
        textarea.value = newValue;
        textarea.focus();
        textarea.selectionStart = textarea.selectionEnd = startIndex + 1 + newWord.length;
    };

    ['input', 'selectionchange'].forEach(e =>
        textarea.addEventListener(e, () => {
            const currentValue = textarea.value;
            const cursorPos = textarea.selectionStart;
            const startIndex = findIndexOfCurrentWord();
    
            const lineEnd = currentValue.substring(cursorPos).split(/\r?\n/g)[0];
            const tags = lineEnd.match(/%input:\w*/g)
            if (tags === null) {
                suggestionsEle.style.display = 'none';
                return;
            }
            const groups = tags.map(x => x.substring(7));
            const suggestions = groups.map((x) => pkmn_data[x]).filter((x) => x !== undefined).flat(1);
    
            // Extract just the current word
            const currentWord = currentValue.substring(startIndex + 1, cursorPos);
            if (currentWord === '') {
                suggestionsEle.style.display = 'none';
                return;
            }
    
            const matches = suggestions.filter((suggestion) => /*suggestion.indexOf(currentWord) > -1*/ suggestion.toLowerCase().startsWith(currentWord.toLowerCase()));
            if (matches.length === 0) {
                suggestionsEle.style.display = 'none';
                return;
            }
    
            const textBeforeCursor = currentValue.substring(0, cursorPos);
            const textAfterCursor = currentValue.substring(cursorPos);
    
            const pre = document.createTextNode(textBeforeCursor);
            const post = document.createTextNode(textAfterCursor);
            const caretEle = document.createElement('span');
            caretEle.innerHTML = '&nbsp;';
    
            mirroredEle.innerHTML = '';
            mirroredEle.append(pre, caretEle, post);
    
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