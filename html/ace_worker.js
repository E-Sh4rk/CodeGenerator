importScripts("ace_js.bc.js?2");

self.postMessage({ type: "ready" });

self.onmessage = function (event) {
  const msg = event.data;
  try {
    if (typeof aceGen === "undefined") {
      throw new Error("Generator failed to load");
    }
    const result = msg.next
      ? aceGen.buildNext(msg.lang, msg.game, msg.code, msg.exitCodes)
      : aceGen.build(msg.lang, msg.game, msg.code, msg.exitCodes);
    const txt = result && result[1] != null ? String(result[1]) : "";
    self.postMessage({ id: msg.id, ok: true, txt });
  } catch (error) {
    self.postMessage({
      id: msg.id,
      ok: false,
      error: error && error.message ? error.message : String(error),
    });
  }
};
