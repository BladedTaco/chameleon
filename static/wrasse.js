import { Terminal as xTerminal } from 'xterm'
import { FitAddon } from 'xterm-addon-fit';
import ESC from './ansiEscapes';

let initialized = false;
const WrasseTerminal = new xTerminal({
    convertEol: true,
    scrollback: 1,
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
    buttons : [document.getElementById('wrasse_0'), document.getElementById('wrasse_1'), document.getElementById('wrasse_2'), document.getElementById('wrasse_3'), document.getElementById("wrasse_tree")]
}


let scrollToTop = () => {
  wrasse.terminal.scrollToTop()
}

let wrasse_setup = () => {
    console.log('wrasse init')

    wrasse.terminal.open(html.terminal);

    initialized = true;

    // ignore all keypresses
    // wrasse.terminal.attachCustomKeyEventHandler((_) => false);
    // wrasse.terminal.modes.mouseTrackingMode
    wrasse.terminal.modes.wraparoundMode = true;
    


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

    html.buttons[4].addEventListener('click', _ => {
      wrasse.interactive_terminal(wrasse.tree)
    });

    // Make the terminal's size and geometry fit the size of #terminal-container
    fitAddon.fit();
    wrasse.terminal.resize(wrasse.terminal.cols, 1000);

    // write starter text
    wrasse.terminal.write('Hello from \x1B[1;3;31mxterm.js\x1B[0m $ ', scrollToTop)
}

let hook = async ({code, response}) => {
    let data = await response;

    console.log('back-end call')
    if (!initialized) {
        wrasse.setup()
    } 

    // send code to ghc handler
    let ghc_data = await handle_ghc(code);

    console.log(ghc_data)

    wrasse.tree = ghc_data.full
    wrasse.data_0 = ghc_data
    wrasse.data_1 = data
    wrasse.data_2 = { ghc: ghc_data, chameleon: data }

    console.log(wrasse.tree)
    switch_terminal(wrasse.data_0)
}


let switch_terminal = (data) => {
    wrasse.terminal.reset()
    wrasse.terminal.options.disableStdin = true;
    wrasse.terminal.writeln(JSON.stringify(
        data,
        null,
        2
    ), scrollToTop)

    // Make the terminal's size and geometry fit the size of #terminal-container
    fitAddon.fit();
}


let interactive_terminal = (tree) => {
  // clear terminal
  wrasse.terminal.reset()

  // tree has format [[curr string, active], [recursive children]]

  // // get lines to write
  // let recurse_getlines = (node, level) => {
  //   if (node[0][1]) {
  //     return ["\t".repeat(level) + node[0][0]] 
  //       + node[1].reduce((prev, curr) => prev + recurse_getlines(curr, level+1), [])
  //   } else {
  //     return ["\t".repeat(level) + node[0][0]]
  //   }
  // }

  // let lines = recurse_getlines(tree, 0)

  let linkProviders = [];

  let curr_line = 1

  let register_tree = (node, line, level) => {
    // write line
    
    let prefix = "  ".repeat(level) 
    let node_string = node[0][0] 
    // wrasse.terminal.writeln(ESC.colouredText({r:100, g:200, b:100}, {}, node_string));

    // write tree string
    if (node[0][1]) {
      if (node[1].length > 0) {
        wrasse.terminal.writeln(prefix + "V " + node_string);
      } else {
        wrasse.terminal.writeln(prefix + "- " + node_string);
      }
    } else {
      if (node[1].length > 0) {
        wrasse.terminal.writeln(prefix + "> " + node_string);
      } else {
        wrasse.terminal.writeln(prefix + "- " + node_string);
      }
    }
    
    curr_line++;


    // register link provider
    linkProviders.push(wrasse.terminal.registerLinkProvider({
      provideLinks(bufferLineNumber, callback) {
        callback([
          {
            text: node_string,
            
            range: { start: { x: prefix.length + 1, y: line }, end: { x: prefix.length + 2 + node_string.length, y: line } },
            activate() {
              // remove all link providers
              linkProviders.forEach((x) => x.dispose())
              node[0][1] = !node[0][1];
              interactive_terminal(wrasse.tree)
            }
            //hover, leave
          }
        ]);
        return;
        // fallthrough failure state
        callback(undefined);
      }
    }));

    // recurse if active
    if (node[0][1]) {
      node[1].forEach(x => register_tree(x, curr_line, level+1))
    }
  }

  register_tree(tree, curr_line, 0);

}



/*


  let register_tree = (node, line, level) => {
    // write line
    let node_string = "\t".repeat(level) + node[0]
    wrasse.terminal.writeln(node_string)
    // register link provider
    linkProviders.push(wrasse.terminal.registerLinkProvider({
      provideLinks(bufferLineNumber, callback) {
        callback([
          {
            text: node_string,
            
            range: { start: { x: 1, y: line }, end: { x: 1 + node_string.length, y: line } },
            activate() {
              wrasse.terminal.write(ESC.cursorTo(1, line + 1))
              wrasse.terminal.write(ESC.insertLine(node[1].length))
              node[1].forEach(element => {
                register_tree(element, ++line, level+1)
              });
            }
            //hover, leave
          }
        ]);
        return;
        // fallthrough failure state
        callback(undefined);
      }
    }));
  }
*/



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
    "tree" : [],
    "data_0" : {},
    "data_1" : {},
    "data_2" : {},
    "switch_terminal" : switch_terminal,
    "interactive_terminal" : interactive_terminal
};

/*
underlined on hover
\x1b[3mVS Code\x1b[0m

https://xtermjs.org/


decorations
function addDecoration(term) {
  const marker = term.addMarker(15);
  const decoration = term.registerDecoration({ marker, x: 44 });
  decoration.onRender(element => {
    element.classList.add('link-hint-decoration');
    element.innerText = 'Try clicking italic text';
    // must be inlined to override inlined width/height coming from xterm
    element.style.height = '';
    element.style.width = '';
  });
}



data input

    term.onData(e => {
      switch (e) {
        case '\u0003': // Ctrl+C
          term.write('^C');
          prompt(term);
          break;
        case '\r': // Enter
          runCommand(term, command);
          command = '';
          break;
        case '\u007F': // Backspace (DEL)
          // Do not delete the prompt
          if (term._core.buffer.x > 2) {
            term.write('\b \b');
            if (command.length > 0) {
              command = command.substr(0, command.length - 1);
            }
          }
          break;
        default: // Print all other characters for demo
          if (e >= String.fromCharCode(0x20) && e <= String.fromCharCode(0x7E) || e >= '\u00a0') {
            command += e;
            term.write(e);
          }
      }
    });


link hanlding
    // Create a very simple link provider which hardcodes links for certain lines
    term.registerLinkProvider({
      provideLinks(bufferLineNumber, callback) {
        switch (bufferLineNumber) {
          case 2:
            callback([
              {
                text: 'VS Code',
                range: { start: { x: 28, y: 2 }, end: { x: 34, y: 2 } },
                activate() {
                  window.open('https://github.com/microsoft/vscode', '_blank');
                }
              },
              {
                text: 'Hyper',
                range: { start: { x: 37, y: 2 }, end: { x: 41, y: 2 } },
                activate() {
                  window.open('https://github.com/vercel/hyper', '_blank');
                }
              },
              {
                text: 'Theia',
                range: { start: { x: 47, y: 2 }, end: { x: 51, y: 2 } },
                activate() {
                  window.open('https://github.com/eclipse-theia/theia', '_blank');
                }
              }
            ]);
            return;
          case 8:
            callback([
              {
                text: 'WebGL renderer',
                range: { start: { x: 54, y: 8 }, end: { x: 67, y: 8 } },
                activate() {
                  window.open('https://npmjs.com/package/xterm-addon-webgl', '_blank');
                }
              }
            ]);
            return;
          case 14:
            callback([
              {
                text: 'Links',
                range: { start: { x: 45, y: 14 }, end: { x: 49, y: 14 } },
                activate() {
                  window.alert('You can handle links any way you want');
                }
              },
              {
                text: 'themes',
                range: { start: { x: 52, y: 14 }, end: { x: 57, y: 14 } },
                activate() {
                  isBaseTheme = !isBaseTheme;
                  term.setOption('theme', isBaseTheme ? baseTheme : otherTheme);
                  document.querySelector('.demo .inner').classList.toggle('other-theme', !isBaseTheme);
                  term.write(`\r\nActivated ${isBaseTheme ? 'xterm.js' : 'snazzy'} theme`);
                  prompt(term);
                }
              },
              {
                text: 'addons',
                range: { start: { x: 60, y: 14 }, end: { x: 65, y: 14 } },
                activate() {
                  window.open('/docs/guides/using-addons/', '_blank');
                }
              }
            ]);
            return;
          case 15: callback([
            {
              text: 'typed API',
              range: { start: { x: 45, y: 15 }, end: { x: 53, y: 15 } },
              activate() {
                window.open('https://github.com/xtermjs/xterm.js/blob/master/typings/xterm.d.ts', '_blank');
              }
            },
            {
              text: 'decorations',
              range: { start: { x: 56, y: 15 }, end: { x: 66, y: 15 } },
              activate() {
                window.open('https://github.com/xtermjs/xterm.js/blob/master/typings/xterm.d.ts#L947', '_blank');
              }
            },
          ]);
            return;
        }
        callback(undefined);
      }
    });
  }

*/

export default wrasse;