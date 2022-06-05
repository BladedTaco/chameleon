import React from 'react';
import ReactDOM from 'react-dom';
import { Provider, useSelector, useDispatch } from 'react-redux';
import store from './store';
import {
  editorModes,
  nextStep,
  prevStep,
  toggleMultipleExps,
  toggleDebuggerStpes,
  switchTaskThunk,
  showOnlyMark1,
  showOnlyMark2,
  showBoth,
  toNormalMode,
  debuggingLevel1,
  debuggingLevel2,
  debuggingLevel3
} from './debuggerSlice';
import Splitter from '@devbookhq/splitter'
import Editor from "./Editor"
import Debugger from "./Debugger"
import MenuBar from "./MenuBar"
import Modal from 'react-modal';
import Split from 'split-grid'

import xterm from 'xterm'


// --------------------------------------------------------------
// --------------------------------------------------------------

import wrasse from './wrasse'

wrasse.terminal.options.convertEol = true
wrasse.terminal.options.tabStopWidth = 2

let col_num = Math.ceil(window.innerWidth / 4)
// let row_num = Math.ceil(window.innerHeight / wrasse.terminal.options.lineHeight)
let row_num = 22 //TODO: make this dynamic

wrasse.terminal.resize(col_num, row_num)
wrasse.terminal.open(document.getElementById('terminal'));
wrasse.terminal.write('Hello from \x1B[1;3;31mxterm.js\x1B[0m $ ')

document.getElementById('wrasse_0').addEventListener('click', _ => {
  wrasse.switch_terminal(wrasse.data_0)
});
document.getElementById('wrasse_1').addEventListener('click', _ => {
  wrasse.switch_terminal(wrasse.data_1)
});
document.getElementById('wrasse_2').addEventListener('click', _ => {
  wrasse.switch_terminal(wrasse.data_2)
});

// --------------------------------------------------------------
// --------------------------------------------------------------

Split({
  columnGutters: [{
    track: 1,
    element: document.querySelector('#gutter'),
  }],
})

Modal.setAppElement('#debugger');

let currentTask = 0;
let currentRound = 1;
let editor = initializeEditor(tasks[currentTask]);
store.dispatch(switchTaskThunk(currentTask));

  
Modal.setAppElement('#react-root');
store.dispatch(switchTaskThunk(0));

window.addEventListener('keyup', (event) => {
  const keyName = event.key;
  let state = store.getState()

  if (state.debugger.mode === editorModes.normal && (keyName === '1' || keyName === '2')) {
    store.dispatch(showBoth())
  }
})

window.addEventListener('keydown', (event) => {
  let state = store.getState()
  const keyName = event.key;
  if (keyName === 'Tab') {
    event.preventDefault()
    if (!state.debugger.multipleExps) {
      store.dispatch(toggleMultipleExps())
    } else if (state.debugger.multipleExps && !state.debugger.debuggingSteps) {
      store.dispatch(toggleDebuggerStpes())
    } else if (state.debugger.multipleExps && state.debugger.debuggingSteps) {
      store.dispatch(toggleMultipleExps())
      store.dispatch(toggleDebuggerStpes())
    }
  }
  if (state.debugger.mode === editorModes.normal) {

document.getElementById('save').addEventListener('click', _ => {
  let text = editor.getValue();
  // store.dispatch(typeCheckThunk(text));
  // --------------------------------------------------------------
  // --------------------------------------------------------------
  // wrasse.hook("he");
  wrasse.hook(store.dispatch(typeCheckThunk(text)));
  
  // --------------------------------------------------------------
  // --------------------------------------------------------------
});

    if (keyName === '1') {
      store.dispatch(showOnlyMark1())
    }

    if (keyName === '2') {
      store.dispatch(showOnlyMark2())
    }

    if (state.debugger.debuggingSteps) {
      if (keyName === 'ArrowDown' || keyName === "ArrowRight" || keyName === 'j') {
        store.dispatch(prevStep())
      }
      if (keyName === 'ArrowUp' || keyName === "ArrowLeft" || keyName === 'k') {
        store.dispatch(nextStep())
      }
    }
  }
})

const App = () => {
  let wellTyped = useSelector(state => state.debugger.wellTyped);

  return <>
    <Modal
      isOpen={wellTyped}
      className='max-w-2xl bg-gray-100 h-80 min-w-max left-1/2 top-1/2 -translate-y-1/2 -translate-x-1/2 absolute p-6 rounded-md'
    >
      <ModelContent />
      {/* this is for study only  */}
    </Modal>
    <div className='w-full h-full flex flex-col'>
      <MenuBar></MenuBar>
      <div className='flex-grow'>
        <Splitter initialSizes={[60, 40]}>
          <Editor></Editor>
          <Debugger></Debugger>
        </Splitter>
      </div>
    </div>
  </>
}

const ModelContent = () => {
  let dispatch = useDispatch();
  let currentTaskNum = useSelector(state => state.debugger.currentTaskNum);
  // analytics.track(events.succeed, { taskNumber: currentTaskNum, mode });
  return (
    <div className='flex flex-col justify-around items-center h-full'>
      <div>
        <p className='text-center'>
          Congratulations. You fixed the type error!
        </p>
        {currentTaskNum === 8 ? (
          <p className='text-center'>Click next to leave us some feedback.</p>
        ) : (
          <p className='text-center'>
            Click next to head over to the next challenge.
          </p>
        )}
      </div>
      <button
        className='px-5 py-1 bg-green-400 rounded-md'
        onClick={() => {
          if (currentTaskNum === 8) {
            window.location =
              'https://forms.gle/nts9EQsrbNFAPEdv8';
            return;
          } else {
            dispatch(switchTaskThunk(currentTaskNum + 1))
            dispatch(toNormalMode())
          }
        }
        }
      >
        Next
      </button>
    </div>
  );
};

ReactDOM.render(
  <React.StrictMode>
    <Provider store={store}>
      <App></App>
    </Provider>
  </React.StrictMode>
  ,
  document.getElementById('react-root'),
);
