
// const backendUrl =
// __SNOWPACK_ENV__.MODE === 'development' ? 'http://localhost:3000' : '';

import xterm from './_snowpack/pkg/xterm.js'


const WrasseTerminal = new xterm.Terminal();

const html = {
    wrasse : document.getElementById('wrasse')
}

let hook = async (response) => {
    let data = await response;

    // send code to ghc handler
    let ghc_data = await handle_ghc(data.meta.arg);

    // // set wrasse window to response json data
    // html.wrasse.innerHTML = JSON.stringify(
    //     { ghc: ghc_data, typecheck: data },
    //     null,
    //     2
    // )
    
    ghc_data.console = ghc_data.console.replace("\n", "\r\n")

    // term.open(document.getElementById('terminal'));
    WrasseTerminal.reset()
    WrasseTerminal.options.disableStdin = true;
    WrasseTerminal.writeln(JSON.stringify(
        { ghc: ghc_data, typecheck: data },
        null,
        2
    ))

    // still figuring this out, not sure if its needed
    // editor = CodeMirror(document.getElementById('wrasse'), {
    //     lineNumbers: true,
    //     mode: null,
    //     value: code,
    // });
}

let handle_ghc = async (code) => {
    return ghc_hook(code)
}

let ghc_hook = async (code) => {
    let response = await fetch('/ghc', {
        method: 'POST',
        body: code,
    });
    return response.json();
}

const wrasse = {
    "hook": hook,
    "terminal": WrasseTerminal
};









export default wrasse;