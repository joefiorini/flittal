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

  function preventDefault(event) {
    event.preventDefault();
    return Maybe.Just(Utils._Tuple0);
  }

  function stopPropagation(event) {
    event.bubbles = false;
    return Maybe.Just(Utils._Tuple0);
  }

  return Elm.Native.Custom.Html.values = {
    getTargetId: getTargetId,
    preventDefault: preventDefault,
    stopPropagation: stopPropagation
  };
};