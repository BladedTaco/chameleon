

let hook = async (response) => {
    let data = await response;
    document.getElementById('wrasse').innerHTML = JSON.stringify(
        data,
        null,
        2
    )
    
    editor = CodeMirror(document.getElementById('wrasse'), {
        lineNumbers: true,
        mode: null,
        value: code,
    });
}

const wrasse = {
    "hook": hook
};

export default wrasse;