window.addEventListener ("load", () => {
    let output = document.getElementById ("output");
    let input = document.getElementById ("input");
    let button = document.getElementById ("enter");

    input.addEventListener ("keyup", event => {
        if(event.key !== "Enter") return;
        button.click();
        event.preventDefault();
    });

    function checkOutputHeight(){
        if(output.selectionStart == output.selectionEnd) {
            output.scrollTop = output.scrollHeight;
        }
     }
     
    setInterval(checkOutputHeight, 500);

});