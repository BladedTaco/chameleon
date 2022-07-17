import { Terminal as xTerminal } from 'xterm'
import { FitAddon } from 'xterm-addon-fit';
import ESC from './ansiEscapes';

let initialized = false;
const WrasseTerminal = new xTerminal({
    convertEol: true,
    scrollback: 1,
    cursorBlink: false,
    cursorStyle: "bar",
    cursorWidth: 1,
    disableStdin: true,
    tabStopWidth: 2,
    //font
    //windowsMode
});
const fitAddon = new FitAddon();
WrasseTerminal.loadAddon(fitAddon);

const html = {
    terminal : document.getElementById('terminal-container'),
    buttons : [
      document.getElementById('wrasse_0'),
      document.getElementById('wrasse_1'), 
      document.getElementById('wrasse_2'), 
      document.getElementById('wrasse_3'), 
      document.getElementById("wrasse_tree")
    ],
    hover : {
      shell : document.getElementById("wrasse-hover"),
      content : document.getElementById("wrasse-hover-content"),
    },
    block: document.getElementById("wrasse-block"),
}


let sleep = async (time) => {
  await new Promise(r => setTimeout(r, time));
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
    wrasse.terminal.modes.mouseTrackingMode = "any"
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

    // setup mouse followers
    const onMouseMove = (e) =>{
      html.hover.shell.style.left = e.pageX + 'px';
      html.hover.shell.style.top = e.pageY + 'px';

      let off = 0;
      if (html.block.classList.contains("hidden")) {
        off = 100;
      }

      html.block.style.left = (e.pageX - off) + 'px';
      html.block.style.top = (e.pageY - off) + 'px';
    }

    document.addEventListener('mousemove', onMouseMove);
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

    wrasse.tree = parse_tree(ghc_data.full)
    wrasse.data_0 = ghc_data
    wrasse.data_1 = data
    wrasse.data_2 = { ghc: ghc_data, chameleon: data }

    console.log(wrasse.tree)
    switch_terminal(wrasse.data_0)
}

let parse_tree = (tree) => {
  return {
    content: tree[0][0],
    active: tree[0][1],
    line: tree[0][2],
    children: tree[1].map(parse_tree)
  }
}


let switch_terminal = (data) => {
  // clean disposables
    perm.disposables.forEach(x => x.dispose())
    perm.disposables = [];

    wrasse.terminal.reset()
    wrasse.terminal.options.disableStdin = true;
    wrasse.terminal.writeln(JSON.stringify(
        data,
        null,
        2
    ), scrollToTop)

    // Make the terminal's size and geometry fit the size of #terminal-container
    fitAddon.fit();

    // star drawing
    let starGen = (function *() {
      while (true) {
        for (const item of ["|", "/", "-", "\\"]) {
          yield item;
       }
      }
    })();

    let numGen = (function *() {
      let i = 0
      while (true) {
        yield ++i;
      }
    })();
    
    let recurse = async () => {
      sleep(100);
      wrasse.terminal.write(ESC.cursorSavePosition 
        + ESC.cursorTo((numGen.next().value % 2) + 30, 1) + starGen.next().value
        + ESC.cursorRestorePosition
        , recurse)
    }
    // recurse();
}


const set_hover_content = (text) => {
  // hide/show based on if text is provided
  if (typeof text === 'undefined' || text === "") {
    // hide element
    html.hover.shell.classList.add("hidden")
  } else { 
    // update text and show element
    html.hover.content.innerText = text
    html.hover.shell.classList.remove("hidden")
  }
}

const block_mouse = (bool) => {
  if (!bool) {
    // hide element
    html.block.classList.add("hidden")
  } else { 
    // show element
    html.block.classList.remove("hidden")
  }
}


let interactive_terminal = (tree) => {
  // clear terminal
  // wrasse.terminal.reset()
  wrasse.terminal.clear()

  // let parse_tree = (tree) => {
  //   return {
  //     content: tree[0][0],
  //     active: tree[0][1],
  //     line: tree[0][2],
  //     children: tree[1].map(parse_tree)
  //   }
  // }
  
  let curr_line = 1;

  let write_text = (node, level, write = false) => {
    let line = curr_line

    let prefix = "  ".repeat(level) 
    let node_string = node.content //JSON.stringify(node)// node.content
    // wrasse.terminal.writeln(ESC.colouredText({r:100, g:200, b:100}, {}, node_string));

    // write tree string
    if (write) {
      if (node.children.length == 0) {
        wrasse.terminal.writeln(prefix + "- " + node_string);
      } else {
        if (node.active) {
          wrasse.terminal.writeln(prefix + "V " + node_string);
        } else {
          wrasse.terminal.writeln(prefix + "> " + node_string);
        }
      }
    }

    node.line = line;
    
    curr_line++;

    // recurse if active
    if (node.active) {
      node.children.forEach(x => write_text(x, level+1, write))
    }
  }

  let clean_lines = (node, level, mode) => {

    // first call
    if (mode === undefined) {
      // set mode
      mode = node.active;
    // else exit on inactive node
    } else if (node.active == false) return;

    if (mode) {
      // delete child lines
      node.children.slice().reverse().forEach(x => clean_lines(x, level+1, mode))

      // delete own lines.
      wrasse.terminal.write(
        ESC.cursorSavePosition +
        ESC.cursorTo(0, node.line) +
        ESC.deleteLine(node.children.length) +
        ESC.cursorTo(2*level, node.line - 1) + ">" +
        ESC.cursorRestorePosition
      )
    } else {
      ///write in new lines
      
      // insert childnum empty lines
      wrasse.terminal.write(
        ESC.cursorSavePosition +
        ESC.cursorTo(0, node.line) +
        ESC.insertLine(node.children.length)
      )

      let curr_line = node.line;
      // write each child line
      node.children.forEach(x => {
        let prefix = "  ".repeat(level+1)
        if (x.children.length == 0) {
          wrasse.terminal.write(
            ESC.cursorTo(0, curr_line) +
            prefix + "- " + x.content
          );
        } else {
          wrasse.terminal.write(
            ESC.cursorTo(0, curr_line) +
            prefix + "> " + x.content
          );
        }
        curr_line++;
      });

      // restore cursor
      wrasse.terminal.write(
        ESC.cursorTo(2*level, node.line - 1) + "V" +
        ESC.cursorRestorePosition
      )

      // write child lines
      node.children.forEach(x => clean_lines(x, level+1, mode))
    }
  }

  let register_links = (node, level, ignoreLine) => {
    // register link provider
    if (node.children.length > 0 && node.line != ignoreLine) {
      const disp = wrasse.terminal.registerLinkProvider({
        provideLinks(bufferLineNumber, callback) {
          callback([
            {
              text: node.content,
              range: { 
                start: { x: level*2+ 1,                        y: node.line },
                end:   { x: level*2 + 2 + node.content.length, y: node.line } 
              },
              activate() {
                // remove all link providers except this one
                perm.disposables
                  .filter(x => x !== disp)
                  .forEach((x) => x.dispose())
                perm.disposables = [disp];

                // fix terminal output
                clean_lines(node, level)

                // flip state
                node.active = !node.active;

                // update tree state
                curr_line = 1
                write_text(tree, 0, false)
                register_links(tree, 0, bufferLineNumber);

                wrasse.terminal.write(ESC.cursorTo(0, bufferLineNumber-1));
              },
              hover() {
                wrasse.set_hover_content(`${Math.random()}
                
                This is a test text, it is normally found when 
                I havent implemented something.`)
              },
              leave() {
                wrasse.set_hover_content()
              }
            }
          ]);
          return;
          // fallthrough failure state
          callback(undefined);
        }
      })
      
      // push disposable
      perm.disposables.push(disp);
    }

    if (node.active) {
      node.children.forEach(x => register_links(x, level+1))
    }
  }

  write_text(tree, 0, true);

  register_links(tree, 0)
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


const perm = {
  "disposables": []
}


const wrasse = {
    "hook": hook,
    "setup": wrasse_setup,
    "terminal": WrasseTerminal,
    "tree" : {},
    "data_0" : {},
    "data_1" : {},
    "data_2" : {},
    "switch_terminal" : switch_terminal,
    "interactive_terminal" : interactive_terminal,
    "set_hover_content" : set_hover_content,
    "block_mouse" : block_mouse,
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