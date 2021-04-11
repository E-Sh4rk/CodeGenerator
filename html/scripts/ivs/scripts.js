window.addEventListener ("load", () => {
    let output = document.getElementById ("output");
    let input = document.getElementById ("input");
    let button = document.getElementById ("enter");

    input.addEventListener ("keyup", event => {
        if(event.key !== "Enter") return;
        button.click();
        event.preventDefault();
    });

    let old_content = "";
    function checkOutputHeight(){
        if (old_content != output.value) {
            if(output.selectionStart == output.selectionEnd) {
                output.scrollTop = output.scrollHeight;
            }
            old_content = output.value;
        }
     }
     
    setInterval(checkOutputHeight, 500);

});