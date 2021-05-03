exports.asCanvasElement = function (el) {
  return function (Just) {
    return function (Nothing) {
      return function () {
        if (el instanceof HTMLCanvasElement) {
          // hack to anchor the width & height on the client
          el.width = el.clientWidth;
          el.height = el.clientHeight;
          return Just(el);
        } else {
          return Nothing;
        }
      };
    };
  };
};
