window.addEventListener ("load", () => {
    let code = document.getElementById ("main");
    let exit_codes = document.getElementById ("secondary");
    let select = document.getElementById ("select");
    let lock = false;

    function getFile(url, field, callback) {
        lock = true;
        let xhr = new XMLHttpRequest();
        xhr.open("GET", url);
        xhr.overrideMimeType("text/plain");
        xhr.addEventListener("readystatechange", () => {
            if (xhr.readyState == 4) {
                if (xhr.status == 200)  {
                    field.value = xhr.responseText;
                } else {
                    console.log("Unknown file " + url);
                }
                lock = false;
                if (callback) callback();
            }
        });
        xhr.send();
    };

    select.addEventListener ("change", () => {
        if (!lock) {
            if (select.value) {
                getFile("files/" + select.value + ".txt", code, null);
            }
            else {
                code.value = "";
            }
        }
    });

    getFile("files/exit.txt", exit_codes, () => {
        getFile("files/empty.txt", code, null);
    });
});