import React from 'react';
import ReactDOM from 'react-dom';
import App from './App';
import './index.css';

fetch('/api/start-data').then((response) => {
  response.json().then(function(data) {
    ReactDOM.render(
      <App {...data} />,
      document.getElementById('root')
    );
  });
});


