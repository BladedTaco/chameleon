
const backendUrl =
__SNOWPACK_ENV__.MODE === 'development' ? 'http://localhost:3000' : '';


const html = {
    wrasse : document.getElementById('wrasse')
}

let hook = async (response) => {
    let data = await response;

    // set wrasse window to response json data
    html.wrasse.innerHTML = JSON.stringify(
        data,
        null,
        2
    )
    
    // send code to ghc handler
    html.wrasse.innerHTML = handle_ghc(data.meta.arg)

    // still figuring this out, not sure if its needed
    // editor = CodeMirror(document.getElementById('wrasse'), {
    //     lineNumbers: true,
    //     mode: null,
    //     value: code,
    // });
}

let handle_ghc = (code) => {
    return ghc_hook(code)
}

let ghc_hook = (code) => {
    let response = await fetch(backendUrl + '/ghc', {
        method: 'POST',
        body: code,
    });
    return response.json();
}

const wrasse = {
    "hook": hook
};

export default wrasse;