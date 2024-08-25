const invisibleChars = {
  160: 'NO-BREAK SPACE',
  173: 'SOFT HYPHEN',
  847: 'UNIVERSAL INTERROGATION',
  1536: 'ARABIC NUMBER SIGN',
  1537: 'ARABIC SIGN SANAH',
  1538: 'ARABIC FOOTNOTE MARKER',
  1539: 'ARABIC SIGN SAFHA',
  1757: 'ARABIC DATE SEPARATOR',
  1807: 'MONGOLIAN VOWEL SEPARATOR',
  3852: 'ORTHOGRAPHIC SYLLABARY SIGN',
  4447: 'HIRAGANA VOWEL SIGN',
  4448: 'KATAKANA VOWEL SIGN',
  5760: 'OGHAM SPACE MARK',
  6155: 'ARABIC HUNDRED MARK',
  6156: 'ARABIC THOUSAND MARK',
  6157: 'ARABIC MILLION MARK',
  6158: 'ARABIC CRORE MARK',
  8192: 'EN QUAD',
  8193: 'EM QUAD',
  8194: 'EN SPACE',
  8195: 'EM SPACE',
  8196: 'THREE-PER-EM SPACE',
  8197: 'FOUR-PER-EM SPACE',
  8198: 'SIX-PER-EM SPACE',
  8199: 'FIGURE SPACE',
  8200: 'PUNCTUATION SPACE',
  8201: 'THIN SPACE',
  8202: 'HAIR SPACE',
  8203: 'ZERO WIDTH SPACE',
  8204: 'ZERO WIDTH NON-JOINER',
  8205: 'ZERO WIDTH JOINER',
  8206: 'LEFT-TO-RIGHT MARK',
  8207: 'RIGHT-TO-LEFT MARK',
  8209: 'NON-BREAKING HYPHEN',
  8232: 'LINE SEPARATOR',
  8233: 'PARAGRAPH SEPARATOR',
  8234: 'LEFT-TO-RIGHT EMBEDDING',
  8235: 'RIGHT-TO-LEFT EMBEDDING',
  8236: 'POP DIRECTIONAL FORMATTING',
  8237: 'LEFT-TO-RIGHT OVERRIDE',
  8238: 'RIGHT-TO-LEFT OVERRIDE',
  8239: 'NARROW NO-BREAK SPACE',
  8260: 'FRACTION SLASH',
  8287: 'MEDIUM MATHEMATICAL SPACE',
  8288: 'WORD JOINER',
  8289: 'FUNCTION APPLICATION',
  8290: 'INVISIBLE TIMES',
  8291: 'INVISIBLE SEPARATOR',
  8292: 'INVISIBLE PLUS',
  8298: 'LEFT-TO-RIGHT ISOLATE',
  8299: 'RIGHT-TO-LEFT ISOLATE',
  8300: 'FIRST STRONG ISOLATE',
  8301: 'POP DIRECTIONAL ISOLATE',
  8302: 'INHIBIT SYMMETRIC SWAPPING',
  8303: 'ACTIVATE SYMMETRIC SWAPPING',
  12272: 'CJK IDEOGRAPHIC SPACE',
  12273: 'HIRAGANA LETTER SMALL A',
  12274: 'HIRAGANA LETTER SMALL I',
  12275: 'HIRAGANA LETTER SMALL U',
  12276: 'HIRAGANA LETTER SMALL E',
  12277: 'HIRAGANA LETTER SMALL O',
  12278: 'HIRAGANA LETTER SMALL KA',
  12279: 'HIRAGANA LETTER SMALL KE',
  12280: 'HIRAGANA LETTER SMALL SA',
  12281: 'HIRAGANA LETTER SMALL SHI',
  12282: 'HIRAGANA LETTER SMALL SU',
  12283: 'HIRAGANA LETTER SMALL SE',
  12288: 'IDEOGRAPHIC SPACE',
  12350: 'CIRCLED DIGIT ZERO',
  65279: 'ZERO WIDTH NO-BREAK SPACE',
  65408: 'HALFWIDTH BLACK SQUARE',
  65532: 'OBJECT REPLACEMENT CHARACTER'
};

function initializeInvisibleCharDetection(selector = 'input[type="text"], textarea') {
  function hasInvisibleChars(text) {
    for (let i = 0; i < text.length; i++) {
      if (invisibleChars.hasOwnProperty(text.charCodeAt(i))) {
        return true;
      }
    }
    return false;
  }

  function handleInputChange(event) {
    const element = event.target;
    const text = element.value;

    if (hasInvisibleChars(text)) {
      if (!element.closest('.invisible-char-wrapper')) {
        createEditableDiv(element);
      }
    } else if (element.closest('.invisible-char-wrapper') && text === '') {
      removeHighlighting(element);
    }
  }

  function createEditableDiv(element) {
    let wrapper = element.closest('div[style*="position: relative"]');
    if (!wrapper) {
      wrapper = document.createElement('div');
      wrapper.style.position = 'relative';
      wrapper.style.display = 'inline-block';
      wrapper.style.width = '100%';
      element.parentNode.insertBefore(wrapper, element);
      wrapper.appendChild(element);
    }

    const inputWrapper = document.createElement('div');
    inputWrapper.className = 'invisible-char-wrapper';
    inputWrapper.style.position = 'relative';
    inputWrapper.style.width = '100%';
    inputWrapper.style.minHeight = `${element.offsetHeight}px`;

    const editableDiv = document.createElement('div');
    editableDiv.className = 'form-control editable-div';
    editableDiv.contentEditable = true;
    editableDiv.innerHTML = highlightInvisibleChars(element.value);
    editableDiv.style.position = 'absolute';
    editableDiv.style.top = '0';
    editableDiv.style.left = '0';
    editableDiv.style.width = '100%';
    editableDiv.style.height = '100%';
    editableDiv.style.padding = getComputedStyle(element).padding;
    editableDiv.style.paddingRight = '30px';
    editableDiv.style.fontSize = getComputedStyle(element).fontSize;
    editableDiv.style.fontFamily = getComputedStyle(element).fontFamily;
    editableDiv.style.lineHeight = getComputedStyle(element).lineHeight;
    editableDiv.style.overflow = 'hidden';

    editableDiv.addEventListener('input', (e) => handleEditableDivInput(e, element));

    inputWrapper.appendChild(editableDiv);
    inputWrapper.appendChild(element);
    wrapper.insertBefore(inputWrapper, wrapper.firstChild);

    // Keep the textarea visible but make it transparent
    element.style.position = 'absolute';
    element.style.top = '0';
    element.style.left = '0';
    element.style.width = '100%';
    element.style.height = '100%';
    element.style.opacity = '0';
    element.style.zIndex = '-1';

    // Move the clear button if it exists
    const clearButton = wrapper.querySelector('.fas.fa-xmark');
    if (clearButton) {
      inputWrapper.appendChild(clearButton);
      clearButton.style.zIndex = '2';
    }

    // Focus the editable div and set the cursor position
    editableDiv.focus();
    setCursorPosition(editableDiv, element.value.length);

    // Initialize autosize for the textarea
    if (typeof autosize === 'function') {
      autosize(element);
    }

    // Sync scroll position
    element.addEventListener('scroll', () => {
      editableDiv.scrollTop = element.scrollTop;
    });
  }

  function handleEditableDivInput(event, originalInput) {
    const editableDiv = event.target;
    const text = editableDiv.innerText;

    if (text === '') {
      removeHighlighting(originalInput);
      return;
    }

    const cursorPosition = getCursorPosition(editableDiv);

    editableDiv.innerHTML = highlightInvisibleChars(text);
    originalInput.value = text;

    setCursorPosition(editableDiv, cursorPosition);

    // Trigger input event on the original input to update clear button visibility
    originalInput.dispatchEvent(new Event('input', { bubbles: true }));

    // Update autosize
    if (typeof autosize === 'function') {
      autosize.update(originalInput);
    }

    syncHeights(editableDiv, originalInput);
  }

  function highlightInvisibleChars(text) {
    let highlightedHTML = '';
    let inSpan = false;

    for (let i = 0; i < text.length; i++) {
      const char = text[i];
      const charCode = text.charCodeAt(i);

      if (invisibleChars.hasOwnProperty(charCode)) {
        if (charCode === 160 || charCode === 8239) { // &nbsp; and &#x2007;
          if (!inSpan) {
            highlightedHTML += '<span class="hl-nbsp">';
            inSpan = true;
          }
        } else {
          if (!inSpan) {
            highlightedHTML += '<span class="hl-evilunichar">';
            inSpan = true;
          }
        }
        const charName = invisibleChars[charCode];
        const hexCode = charCode.toString(16).toUpperCase().padStart(4, '0');
        const spanStyle = [8203, 8204, 8205, 65279].includes(charCode) ? 'style="display: inline-block;width: 0.15em;"' : '';

        highlightedHTML += `<span title="${charName} (U+${hexCode})" ${spanStyle}>${char}</span>`;
      } else {
        if (inSpan) {
          highlightedHTML += '</span>';
          inSpan = false;
        }
        highlightedHTML += char;
      }
    }

    if (inSpan) {
      highlightedHTML += '</span>';
    }

    return highlightedHTML;
  }

  function removeHighlighting(element) {
    const wrapper = element.closest('.invisible-char-wrapper');
    if (wrapper && wrapper.contains(element)) {
      const parentWrapper = wrapper.parentElement;
      parentWrapper.insertBefore(element, wrapper);
      wrapper.remove();
      element.style.display = '';
      element.focus();
    }
  }

  function getCursorPosition(element) {
    const selection = window.getSelection();
    const range = selection.getRangeAt(0);
    const preCaretRange = range.cloneRange();
    preCaretRange.selectNodeContents(element);
    preCaretRange.setEnd(range.endContainer, range.endOffset);
    return preCaretRange.toString().length;
  }

  function setCursorPosition(element, position) {
    const range = document.createRange();
    const sel = window.getSelection();
    let currentPos = 0;
    let found = false;

    function traverseNodes(node) {
      if (node.nodeType === Node.TEXT_NODE) {
        if (currentPos + node.length >= position) {
          range.setStart(node, position - currentPos);
          range.setEnd(node, position - currentPos);
          found = true;
          return;
        }
        currentPos += node.length;
      } else {
        for (let i = 0; i < node.childNodes.length && !found; i++) {
          traverseNodes(node.childNodes[i]);
        }
      }
    }

    traverseNodes(element);

    if (!found) {
      range.selectNodeContents(element);
      range.collapse(false);
    }

    sel.removeAllRanges();
    sel.addRange(range);
  }

  function syncHeights(editableDiv, textarea) {
    const wrapperDiv = editableDiv.closest('.invisible-char-wrapper');
    const textareaHeight = textarea.offsetHeight;
    wrapperDiv.style.height = `${textareaHeight}px`;
    editableDiv.style.height = `${textareaHeight}px`;
  }

  const inputs = document.querySelectorAll(selector);
  inputs.forEach(input => {
    if (hasInvisibleChars(input.value)) {
      createEditableDiv(input);
    }
    input.addEventListener('input', handleInputChange);
  });
}