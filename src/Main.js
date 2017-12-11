'use strict';

var RandExp = require('randexp');

exports.gen = function(randExp) {
  return randExp.gen();
};

exports.randExp = function(str) {
  return new RandExp(str);
};
