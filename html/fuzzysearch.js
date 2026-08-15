const FuzzySearch = (function () {
  function normalize(s) {
    return s
      .toLowerCase()
      .replace(/_/g, " ")
      .replace(/[^\w\s]/g, "")
      .replace(/\s+/g, " ")
      .trim();
  }

  function compact(s) {
    return normalize(s).replace(/\s/g, "");
  }

  function score(query, text, id) {
    if (!query) return 0;

    const q = query.toLowerCase();
    const qNorm = normalize(query);
    const qCompact = compact(query);
    const tLower = text.toLowerCase();
    const tNorm = normalize(text);
    const tCompact = compact(text);

    if (id !== undefined && /^\d+$/.test(q)) {
      const num = parseInt(q, 10);
      if (num === id) return 1000;
      if (String(id).startsWith(q)) return 500 + q.length;
    }

    if (tLower === q) return 900;
    if (tNorm === qNorm) return 850;

    if (tLower.startsWith(q)) return 800;
    if (tNorm.startsWith(qNorm)) return 750;
    if (tCompact.startsWith(qCompact)) return 700;

    if (tLower.includes(q)) return 600;
    if (tNorm.includes(qNorm)) return 550;
    if (tCompact.includes(qCompact)) return 500;

    const qWords = qNorm.split(" ").filter(Boolean);
    const tWords = tNorm.split(" ").filter(Boolean);
    if (qWords.length > 1) {
      let wordScore = 0;
      let matched = 0;
      for (let i = 0; i < qWords.length; i++) {
        const qw = qWords[i];
        const tw = tWords[i];
        if (tw && tw.startsWith(qw)) {
          matched++;
          wordScore += 80;
        }
      }
      if (matched === qWords.length) return 400 + wordScore;
    }

    let qi = 0;
    for (let i = 0; i < tCompact.length && qi < qCompact.length; i++) {
      if (tCompact[i] === qCompact[qi]) qi++;
    }
    if (qi === qCompact.length && qCompact.length > 0) {
      return 100 + (qCompact.length / tCompact.length) * 200;
    }

    return 0;
  }

  function filterAndRank(items, query, getText, getId, limit = 50) {
    if (!query) return [];

    const results = [];
    for (const item of items) {
      const text = getText(item);
      const id = getId ? getId(item) : undefined;
      const s = score(query, text, id);
      if (s > 0) results.push({ item, score: s, text });
    }

    results.sort((a, b) => b.score - a.score || a.text.length - b.text.length);
    return results.slice(0, limit).map((r) => r.item);
  }

  function tomSelectScore(search, text) {
    if (!search || !search.trim()) return 1;
    return score(search, text);
  }

  return { normalize, score, filterAndRank, tomSelectScore };
})();
