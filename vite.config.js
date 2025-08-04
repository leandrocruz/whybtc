import { defineConfig } from "vite";
import scalaJSPlugin from "@scala-js/vite-plugin-scalajs";
import rollupPluginSourcemaps from "rollup-plugin-sourcemaps";

export default defineConfig({
  plugins: [scalaJSPlugin()],
  server: {
    watch: { usePolling: true },
  },
  build: {
    rollupOptions: {
      plugins: [rollupPluginSourcemaps()],
    },
    minify: "esbuild",
    sourcemap: true
  }
});