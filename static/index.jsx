import React from 'react';
import ReactDOM from 'react-dom';
import { Provider } from 'react-redux';
import store from './store';
import {
  editorModes,
  nextStep,
  prevStep
} from './debuggerSlice';
import Splitter, { SplitDirection } from '@devbookhq/splitter'
import Editor from "./Editor"
import Debugger from "./Debugger"
import MenuBar from "./MenuBar"

window.addEventListener('keydown', (event) => {
  let state = store.getState()
  if (state.debugger.mode === editorModes.normal && state.debugger.debuggingSteps) {
    const keyName = event.key;
    if (keyName === 'ArrowDown' || keyName === "ArrowRight") {
      store.dispatch(prevStep())
    }
    if (keyName === 'ArrowUp' || keyName === "ArrowLeft") {
      store.dispatch(nextStep())
    }
  }

})

const App = () => {
  return <div className='w-full h-full flex flex-col'>
    <MenuBar></MenuBar>
    <div className='flex-grow'>
      <Splitter initialSizes={[60, 40]}>
        <Editor></Editor>
        <Debugger></Debugger>
      </Splitter>

    </div>
  </div>
}

ReactDOM.render(
  <React.StrictMode>
    <Provider store={store}>
      <App></App>
    </Provider>
  </React.StrictMode>
  ,
  document.getElementById('react-root'),
);
