import { test, expect } from "vitest";
import { readFileSync } from "node:fs";
import { toUnityShader } from "../src/converters/unity/converter";
import { parseshadertoy } from "../src/utils";

test("unity shader", () => {
    const shadertoyName = "Bubbles";
    const res = readFileSync(
        `./test/fixtures/shadertoy/${shadertoyName}.json`,
        {
            encoding: "utf8",
        },
    );
    const unityShader = readFileSync(
        `./test/fixtures/unity/${shadertoyName}.shader`,
        {
            encoding: "utf8",
        },
    );
    const [shadertoy] = JSON.parse(res);
    const shader = parseshadertoy(shadertoy);
    expect(toUnityShader(shadertoyName, shader)).to.equal(unityShader);
});
