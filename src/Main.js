exports.asCanvasElement = function (el) {
  return function (Just) {
    return function (Nothing) {
      return function () {
        if (el instanceof HTMLCanvasElement) {
          return Just(el);
        } else {
          return Nothing;
        }
      };
    };
  };
};
