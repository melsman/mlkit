/* The compiler starts only after the IDE has had an opportunity to paint. */
(function () {
  "use strict";
  var started = false;
  var report;
  function failed(error) {
    report("[Compiler startup failed: " + String(error) + ". Please reload.]\n");
  }
  function script(path) {
    return new Promise(function (resolve, reject) {
      var element = document.createElement("script");
      element.src = path;
      element.onload = resolve;
      element.onerror = function () { reject(new Error("Failed to load " + path)); };
      document.head.appendChild(element);
    });
  }
  window.smltojsLoading = {
    start: function (initialize, output) {
      if (started) return;
      started = true;
      report = output;
      requestAnimationFrame(function () {
        setTimeout(function () {
          // Fetch the Basis alongside the compiler, but execute it afterwards.
          var preload = document.createElement("link");
          preload.rel = "preload";
          preload.as = "script";
          preload.href = "bundle_extra.js";
          document.head.appendChild(preload);
          // The split debug entry point already includes the compiler scripts.
          var compiler = window.smltojsLoading.compilerLoaded ? Promise.resolve() : script("bundle_compiler.js");
          compiler.then(function () { return script("bundle_extra.js"); })
            .then(function () {
              initialize();
            }).catch(failed);
        }, 0);
      });
    },
    failed: failed
  };
}());
