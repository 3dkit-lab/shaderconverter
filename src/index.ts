import {
    copyFileSync,
    readFileSync,
    writeFileSync,
    existsSync,
    mkdirSync,
} from "node:fs";
import { parseshadertoy } from "@/utils";
import { generate } from "@/converters/unity";

const main = () => {
    const shadertoyName = "Channel";
    const res = readFileSync(
        `./test/fixtures/shadertoy/${shadertoyName}.json`,
        {
            encoding: "utf8",
        },
    );
    const [shadertoy] = JSON.parse(res);
    const shader = parseshadertoy(shadertoy);
    // const unityShader = toUnityShader(shadertoyName, shader);
    // writeFileSync(`./test/fixtures/unity/${shadertoyName}.shader`, unityShader);
    const files = generate(shadertoyName, shader);
    const unityDir = `./test/fixtures/unity/${shadertoyName}/`;
    if (!existsSync(unityDir)) {
        mkdirSync(unityDir, { recursive: true });
    }
    for (const file of files) {
        const { name, content, url } = file;
        if (content) {
            writeFileSync(`${unityDir}${name}`, content);
        } else {
            copyFileSync(`./assets/${url}`, `${unityDir}${name}`);
        }
    }
};

main();
