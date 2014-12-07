Elm.Native.Custom = {};
Elm.Native.Custom.Html = {};

Elm.Native.Custom.Html.make = function(elm) {

  var Maybe = Elm.Maybe.make(elm);
  var Utils = Elm.Native.Utils.make(elm);

  function getTargetId(event) {
      return 'id' in event.target ?
          Maybe.Just(event.target.id) :
          Maybe.Nothing;
  }

  function getMouseSelectionEvent(event) {
    var id = 'id' in event.target ?  event.target.id : '';

    return Maybe.Just({
      id: id,
      metaKey: event.metaKey,
      altKey: event.altKey,
      ctrlKey: event.ctrlKey,
      shiftKey: event.shiftKey
    });
  }

  function preventDefault(event) {
    event.preventDefault();
    return Maybe.Just(Utils._Tuple0);
  }

  function setFocus(selector) {
    return function(chain) {
      setTimeout(function() {
        var nodes = document.querySelectorAll(selector);
        console.log('setting focus in ', selector, nodes);
        if (nodes.length === 1 && document.activeElement !== nodes[0]) {
            nodes[0].focus()
            nodes[0].setSelectionRange(0, nodes[0].value.length)
        }
      }, 100);
      return chain;
    }
  }

  function stopPropagation(event) {
    event.bubbles = false;
    return Maybe.Just(event);
  }

  return Elm.Native.Custom.Html.values = {
    setFocus: setFocus,
    getTargetId: getTargetId,
    preventDefault: preventDefault,
    stopPropagation: stopPropagation,
    getMouseSelectionEvent: getMouseSelectionEvent
  };
};
