var result = {
  id: '',
  isStart: false,
  isDrop: false,
  isEnd: false,
  isMulti: false,
  startX: 0,
  startY: 0,
  endX: 0,
  endY: 0
};

function StyleWithColor() {
  return {
    adapt: function(obj) {
      obj.style = { color: 'white' };
      return obj;
    }
  };
}

document.onkeydown = function(e) {
  if (e.keyCode == 9) {
    e.preventDefault();
  }
};

function adapt(state) {
  var adapters = [StyleWithColor()];
  return adapters.reduce(function(updated, adapter) {
    return adapter.adapt(updated);
  }, state);
}

function runApp(board) {
  function sendPort(port, result) {
    result.isStart = result.isEnd = result.isDrop = false;

    switch (port) {
      case 'dragstart':
        result.isStart = true;
        break;
      case 'dragend':
        result.isEnd = true;
        break;
      case 'drop':
        result.isDrop = true;
        break;
    }

    board.ports[port].send(result);
  }

  if (window.localStorage.getItem('sawIntro') == null) {
    window.localStorage.setItem('sawIntro', true);
    window.location.href = 'https://tinyurl.com/flittal-alpha';
  }

  board.ports.selectInputText.subscribe(inputId => {
    setTimeout(() => {
      const input = document.getElementById(inputId);
      input.setSelectionRange(0, input.value.length);
    });
  });

  const container = document.querySelector('main');

  container.addEventListener(
    'dragstart',
    function(e) {
      result.id = e.target.id;
      result.startX = e.clientX;
      result.startY = e.clientY;

      if (e.metaKey) {
        result.isMulti = true;
      } else {
        result.isMulti = false;
      }

      e.dataTransfer.setData('text/html', null);

      sendPort('dragstart', result);
    },
    false
  );

  container.addEventListener(
    'dragover',
    function(e) {
      e.preventDefault();
    },
    false
  );

  container.addEventListener(
    'dragend',
    function(e) {
      sendPort('dragend', result);
    },
    false
  );

  container.addEventListener(
    'drop',
    function(e) {
      e.stopPropagation();
      e.preventDefault();
      result.endX = e.clientX;
      result.endY = e.clientY;

      sendPort('drop', result);
    },
    false
  );
}

var forms = document.querySelectorAll('form');

window.onsubmit = function(e) {
  e.preventDefault();
};

setTimeout(function() {
  var navBar = document.querySelector('nav.nav-bar');
  navBar.classList.add('nav-bar--compressed');
}, 5000);

function start(flags) {
  var board = Elm.Main.fullscreen(flags);

  window.onload = () => {
    runApp(board);
  };
}

window.startApp = start;
