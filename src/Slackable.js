"use strict";

exports.slackenBuilder = function (dictObjectOf) {
  return function (dictObjectOf1) {
    return function (build) {
      return function (builder) {
        return function (v0) {
          return build(builder)(v0);
        };
      };
    };
  };
};
