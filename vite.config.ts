import { resolve } from "path";
import { defineConfig } from "vite";
import dts from "vite-plugin-dts";
import eslint from "vite-plugin-eslint";

// https://vitejs.dev/guide/build.html#library-mode
export default defineConfig({
  resolve: {
    alias: {
      "@": resolve(__dirname, "src"),
    },
  },
  build: {
    lib: {
      entry: resolve(__dirname, "src/index.ts"),
      name: "shaderconverter",
      fileName: "shaderconverter",
    },
  },
  plugins: [
    dts(),
    eslint({
      include: ["src/**/*.ts", "test/**/*.ts"],
    }),
  ],
});
