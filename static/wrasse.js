
// const backendUrl =
// __SNOWPACK_ENV__.MODE === 'development' ? 'http://localhost:3000' : '';


const html = {
    wrasse : document.getElementById('wrasse')
}

let hook = async (response) => {
    let data = await response;

    // send code to ghc handler
    let ghc_data = await handle_ghc(data.meta.arg);

    // set wrasse window to response json data
    html.wrasse.innerHTML = JSON.stringify(
        { ghc: ghc_data, typecheck: data },
        null,
        2
    )
    

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
    "hook": hook
};

export default wrasse;