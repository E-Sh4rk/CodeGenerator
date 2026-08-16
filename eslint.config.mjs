import js from "@eslint/js";
import prettier from "eslint-config-prettier";
import globals from "globals";

const sharedGlobals = {
  ...globals.browser,
  TomSelect: "readonly",
  aceGen: "readonly",
  repository: "readonly",
  FuzzySearch: "readonly",
  Options: "readonly",
  Highlight: "readonly",
  pkmn_data: "readonly",
  pkmn_data_map: "readonly",
};

export default [
  {
    ignores: ["html/**/*.bc.js", "html/ids.js"],
  },
  {
    files: ["html/**/*.js"],
    languageOptions: {
      ecmaVersion: 2022,
      sourceType: "script",
      globals: sharedGlobals,
    },
    rules: {
      ...js.configs.recommended.rules,
      "no-unused-vars": [
        "error",
        {
          argsIgnorePattern: "^_",
          varsIgnorePattern:
            "^(_|repository|FuzzySearch|Options|Highlight|openTab|compute|computeNext)$",
          caughtErrorsIgnorePattern: "^_",
        },
      ],
      eqeqeq: ["error", "smart"],
      "no-var": "error",
    },
  },
  {
    files: [
      "html/config.js",
      "html/fuzzysearch.js",
      "html/options.js",
      "html/highlight.js",
    ],
    rules: {
      "no-redeclare": "off",
    },
  },
  prettier,
];
