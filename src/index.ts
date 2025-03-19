import { readFileSync, writeFileSync } from "node:fs";
import { parseshadertoy } from "@/utils";
import { toUnityShader } from "@/converters/unity";

const main = () => {
    const shadertoyName = "Selfie Girl";
    const res = readFileSync(
        `./test/fixtures/shadertoy/${shadertoyName}.json`,
        {
            encoding: "utf8",
        },
    );
    const [shadertoy] = JSON.parse(res);
    const shader = parseshadertoy(shadertoy);
    const unityShader = toUnityShader(shadertoyName, shader);
    writeFileSync(`./test/fixtures/unity/${shadertoyName}.shader`, unityShader);
};

main();
