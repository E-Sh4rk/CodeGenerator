
let exit_codes = "@@ filename = \"CertificateFRA\"\n@@ start = 64\n@@\n\nmvn r12, #0xE1                ;  R12=notE1=FFFFFF1E\nbic r12,r12, #0xED00000       ;  R12=R12 and notED00000=F12FFF1E\nbic r11,r12, #0x1000000E      ;  R11=R12 and not1000000E=E12FFF10=bx r0 opcpde\nadcs r12,pc, #0x30            ;  R12=PC+30\nstr r11, [r12, #0]!           ;  Store bx r0 opcode in [r12]\nadc r12,lr, #0xE30            ;  R12=LR+E30\nadc r12,r12, #0xD30000        ;  R12=R12+D30000\nbic r12,r12, #0xC00000        ;  R12=R12 and notC00000\nadc r0,r12, #0xE2             ;  R0=R12+E2=????FRA\n\n====================\n\n@@ filename = \"WhiteOutFRA\"\n@@ start = 64\n@@\n\nmvn r12, #0xE1            ;  R12=notE1=FFFFFF1E\nbic r12,r12, #0xED00000   ;  R12=R12 and notED00000=F12FFF1E\nbic r11,r12, #0x1000000E  ;  R11=R12 and not1000000E=E12FFF10=bx r0 opcpde\nadcs r12,pc, #0x30        ;  R12=PC+30\nstr r11, [r12, #0]!       ;  Store bx r0 opcode in [r12]\nadc r12,lr, #0xA10        ;  R12=LR+A10\nadc r12,r12, #0xD30000    ;  R12=R12+D30000\nbic r12,r12, #0xC00000    ;  R12=R12 and notC00000\nadc r0,r12, #0xC8         ;  R0=R12+C8=SetCB2WhiteOutFRA\n\n====================\n\n@@ filename = \"CertificateShortFRA\"\n@@ start = 92\n@@\n\nadc r12,lr, #0xE30            ;  R12=LR+E30\nadc r12,r12, #0xD30000        ;  R12=R12+D30000\nbic r12,r12, #0xC00000        ;  R12=R12 and notC00000\nadc r0,r12, #0xE2             ;  R0=R12+E2=????FRA\n\n====================\n\n@@ filename = \"CertificateFullFRA\"\n@@ start = 56\n@@\n\nadcs r12, pc, #0x38           ;  R12=&Box14Name + 3\nmvn r11, #0xE1                ;  R11=notE1=FFFFFF1E\nbic r11, r11, #0xED00000      ;  R11=R11 and notED00000=F12FFF1E\nbic r11, r11, #0x1000000E     ;  R11=R11 and not1000000E=E12FFF10=bx r0 opcode\nstr r11, [r12, #0]!           ;  Store bx r0 opcode in [r12]\n\nadc r12,lr, #0xE30            ;  R12=LR+E30\nadc r12,r12, #0xD30000        ;  R12=R12+D30000\nbic r12,r12, #0xC00000        ;  R12=R12 and notC00000\nadc r0,r12, #0xE2             ;  R0=R12+E2=????FRA\n0                             ;  Fill box 14 with space\n\n====================\n\n@@ filename = \"Bootstrapped\"\n@@ start = 116\n@@\n";

function translate(code){
    code = code.replace(/␣/g, "_");
    code = code.replace(/“/g, "«");
    code = code.replace(/”/g, "»");
    code = code.replace(/–/g, "-");
    return code; 
}

function build_code(txt) {
    let res = aceGen.build(txt, exit_codes);
    return res.map(translate);
}

function html_for_code(txt) {
    let res = build_code(txt);
    let arr = res.map(str => "<li>" + str + "</li>");
    return "<ul class=\"ace\">" + arr.join("\n") + "</ul>";
}

function generate(e) {
    let target_id = e.dataset.target;
    let source_id = e.dataset.source;
    let preprocess = e.dataset.preprocess;
    let target = document.getElementById(target_id);
    let source = document.getElementById(source_id);
    let input = preprocess ? eval(preprocess+"(source.innerHTML)") : source.innerHTML;
    target.innerHTML = html_for_code(input);
}
