self.onmessage = function (e) {
  'use strict';

  self.postMessage(e.data);
  self.close();
};
