import React from 'react';
import ReactDOM from 'react-dom';
import App from './App';
import './index.css';

let implText = window.ScalaBigOLibrary.implText;
let dataStructureTexts = window.ScalaBigOLibrary.dataStructureTexts;
let app = window.webapp.WebApp();
let chooser = app.getChooser(implText, dataStructureTexts);

ReactDOM.render(
  <App
      chooser={chooser}
      implText={implText}
      dataStructureTexts={dataStructureTexts}
      library={chooser.library}
      decls={JSON.parse(chooser.declsString)}
      impls={JSON.parse(chooser.implsString)}
      dataStructures={JSON.parse(chooser.structuresString)}
  />,
  document.getElementById('root')
);


