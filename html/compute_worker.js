importScripts("ace_js.bc.js?2");

self.onmessage = function (e) {
  const messageData = e.data;
  try {
    const fn = messageData.op === "buildNext" ? aceGen.buildNext : aceGen.build;
    const result = fn.call(aceGen, messageData.lang, messageData.game, messageData.code, messageData.exitCodes);
    self.postMessage({ id: messageData.id, ok: true, txt: result[1] });
  } catch (err) {
    self.postMessage({ id: messageData.id, ok: false, error: String(err) });
  }
};
