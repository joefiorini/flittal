Elm.Native.App = {};

Elm.Native.App.make = function(elm) {

  function serializeState(state) {
    console.log(JSON.stringify(state));
    var encoded = btoa(JSON.stringify(state));

    window.history.pushState({}, "", "/boards/" + encoded);
    return state;
  }

  return Elm.Native.App.values = {
    serializeState: serializeState
  };

};

