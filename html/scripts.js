window.addEventListener ("load", () => {
    const url = new URL(window.location.href);
    let workspace = document.getElementById ("script_workspace").value;
    let prefix_for_examples = workspace + "/";
    let list_path = prefix_for_examples+"list.json";
    let empty_path = prefix_for_examples+"empty.txt";
    let exit_codes_path = prefix_for_examples+"exit.txt";
    let code = document.getElementById ("main");
    let exit_codes = document.getElementById ("secondary");
    let select = document.getElementById ("select");
    let lang = document.getElementById ("lang");
    let cat = document.getElementById ("cat");
    let game = document.getElementById ("game");

    let lock = false;
    let examples = null;
    let last_selected_example = "";

    function getFile(url, success_callback, callback) {
        lock = true;
        let xhr = new XMLHttpRequest();
        xhr.open("GET", url);
        xhr.setRequestHeader("Cache-Control", "no-cache, no-store, max-age=0");
        xhr.overrideMimeType("text/plain");
        xhr.addEventListener("readystatechange", () => {
            if (xhr.readyState == 4) {
                if (xhr.status == 200)  {
                    success_callback(xhr.responseText);
                } else {
                    console.log("Unknown file " + url);
                }
                lock = false;
                if (callback) callback();
            }
        });
        xhr.send();
    };

    function fileToField(url, field, callback) {
        getFile(url, (content) => {
            let str = content.replaceAll("{LANG}", lang.value.toUpperCase());
            str = str.replaceAll("{lang}", lang.value.toLowerCase());
            str = str.replaceAll("{GAME}", game.value.toUpperCase());
            str = str.replaceAll("{game}", game.value.toLowerCase());
            field.value = str;
        }, callback);
    }

    function updateCode() {
        if (examples == null) return;
        if (!lock) {
            if (select.value) {
                let obj = examples[parseInt(select.value)];
                fileToField(prefix_for_examples + obj[lang.value], code, null);
            }
            else {
                fileToField(empty_path, code, null);
            }
        }
    }

    function updateSelectField() {
        if (examples == null) return;

        let language = lang.value;
        let gam = game.value;
        let category = cat.value;
        //last_selected_example = select.value;
        let newSelectedIndex = 0;
        
        let arrOptions = ["<option value='' selected>----------</option>"];
        let i = 1;
        examples.forEach((element,index) => {
            if (language in element
                && (category == "" || ("cat" in element && element["cat"].includes(category)))
                && (gam == "" || ("game" in element && element["game"].includes(gam)))) {
                let val = index.toString();
                if (val == last_selected_example) newSelectedIndex = i;
                arrOptions.push("<option value='"+val+"'>"+element["name"]+"</option>");
                i += 1;
            }
        });

        select.innerHTML = arrOptions.join("\n");
        select.selectedIndex = newSelectedIndex;

        updateCode();
    }

    function loadExamples(callback) {
        getFile(list_path, (content) => {
            examples = JSON.parse(content);
            updateSelectField();
        }, callback);
    }

    function langChanged() {
        url.searchParams.set('lang', lang.value);
        window.history.replaceState(null, null, url);
        updateSelectField();
    }
    function gameChanged() {
        url.searchParams.set('game', game.value);
        window.history.replaceState(null, null, url);
        updateSelectField();
    }

    let langval = url.searchParams.get('lang');
    if (langval)
        lang.value = langval;

    let gameval = url.searchParams.get('game');
    if (gameval)
        game.value = gameval;

    select.addEventListener ("change", () => {
        last_selected_example = select.value;
        updateCode();
    });

    lang.addEventListener ("change", langChanged);
    game.addEventListener ("change", gameChanged);
    cat.addEventListener ("change", updateSelectField);

    fileToField(exit_codes_path, exit_codes, () => {
        fileToField(empty_path, code, () => {loadExamples(null);});
    });
});

/* ===== TABS ===== */

function openTab(_, id) {
    let active = document.getElementsByClassName("tabcontent-active")[0];
    active.className = active.className.replace("tabcontent-active", "tabcontent");
    let target = document.getElementById(id);
    target.className = target.className.replace("tabcontent", "tabcontent-active");
}
