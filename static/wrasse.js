import { Terminal as xTerminal } from 'xterm'
import { FitAddon } from 'xterm-addon-fit';

let initialized = false;
const WrasseTerminal = new xTerminal({
    convertEol: true,
    cursorBlink: false,
    cursorStyle: "underline",
    disableStdin: true,
    tabStopWidth: 2
    //font
    //windowsMode
});
const fitAddon = new FitAddon();
WrasseTerminal.loadAddon(fitAddon);

const html = {
    terminal : document.getElementById('terminal-container'),
    buttons : [document.getElementById('wrasse_0'), document.getElementById('wrasse_1'), document.getElementById('wrasse_2'), document.getElementById('wrasse_3')]
}

let wrasse_setup = () => {
    console.log('wrasse init')

    wrasse.terminal.open(html.terminal);

    initialized = true;

    // ignore all keypresses
    wrasse.terminal.attachCustomKeyEventHandler((_) => false);
    // wrasse.terminal.modes.mouseTrackingMode
    wrasse.terminal.modes.wraparoundMode = true;
    

    wrasse.terminal.write('Hello from \x1B[1;3;31mxterm.js\x1B[0m $ ')

    html.buttons[0].addEventListener('click', _ => {
        wrasse.switch_terminal(wrasse.data_0)
    });

    html.buttons[1].addEventListener('click', _ => {
        wrasse.switch_terminal(wrasse.data_1)
    });

    html.buttons[2].addEventListener('click', _ => {
        wrasse.switch_terminal(wrasse.data_2)
    });

    html.buttons[3].addEventListener('click', _ => {
        fitAddon.fit();
    });


    // wrasse.terminal.onRender(({start, end}) => {
    //     console.log(start, end)
    //     if (start > 0) {
    //         wrasse.terminal.scrollToTop()
    //     }
    // });

    // Make the terminal's size and geometry fit the size of #terminal-container
    fitAddon.fit();
}

let hook = async ({code, response}) => {
    let data = await response;

    console.log('back-end call')
    if (!initialized) {
        wrasse.setup()
    } 

    // send code to ghc handler
    let ghc_data = await handle_ghc(code);

    wrasse.data_0 = ghc_data
    wrasse.data_1 = data
    wrasse.data_2 = { ghc: ghc_data, chameleon: data }

    switch_terminal(wrasse.data_0)
}


let switch_terminal = (data) => {
    wrasse.terminal.reset()
    wrasse.terminal.options.disableStdin = true;
    wrasse.terminal.writeln(JSON.stringify(
        data,
        null,
        2
    ))

    // Make the terminal's size and geometry fit the size of #terminal-container
    fitAddon.fit();
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
    "setup": wrasse_setup,
    "terminal": WrasseTerminal,
    "data_0" : {},
    "data_1" : {},
    "data_2" : {},
    "switch_terminal" : switch_terminal
};


export default wrasse;