import { renderpass, input, shader, shadertoy } from "@/typing";

export const generateGuid = (): string => {
    return "xxxxxxxxxxxx4xxxyxxxxxxxxxxxxxxx"
        .replace(/[xy]/g, function (c) {
            const r = (Math.random() * 16) | 0;
            const v = c === "x" ? r : (r & 0x3) | 0x8;
            return v.toString(16);
        })
        .toLowerCase();
};

export const parseshadertoy = (res: shadertoy): shader => {
    const { info, renderpass } = res;
    const data = parseRenderpass(renderpass);
    const multipass = renderpass.length > 1;

    const { name } = info;
    return { name, data, multipass };
};

export const parseRenderpass = (renderpasses: renderpass[]) => {
    const renderpass = Object.create({});
    const exportRenderpass = Object.create({});
    let image,
        bufferA,
        bufferB,
        bufferC,
        bufferD,
        common,
        sound,
        cubemap;
    const fileList = Object.create({});
    for (let i = 0; i < renderpasses.length; i++) {
        const pass = renderpasses[i];
        const { name, type, inputs, outputs } = pass;
        if (type == "image") {
            image = pass;
        } else if (type == "buffer") {
            const bufferName = name.replace(" ", "").toLocaleLowerCase();
            if (bufferName == "buffera") {
                bufferA = pass;
            } else if (bufferName == "bufferb") {
                bufferB = pass;
            } else if (bufferName == "bufferc") {
                bufferC = pass;
            } else if (bufferName == "bufferd") {
                bufferD = pass;
            }
        } else if (type == "cubemap") {
            cubemap = pass;
        } else if (type == "common") {
            common = pass;
        } else {
            throw new Error("unknow type!");
        }
        const [output] = outputs;
        if (
            typeof output === "undefined" ||
            type === "image" ||
            type === "common"
        ) {
            continue;
        }

        const { id: outputId } = output;
        exportRenderpass[outputId] = false;
        for (let j = 0; j < inputs.length; j++) {
            const input = inputs[j];
            const { id: inputId, type } = input;
            if (typeof exportRenderpass[inputId] !== "undefined") {
                exportRenderpass[outputId] = true;
            }
            if (
                type == "texture" ||
                type == "volume" ||
                type == "video" ||
                type == "music" ||
                type == "cubemap"
            ) {
                fileList[inputId] = input;
            }
        }
    }
    return Object.assign(renderpass, {
        image,
        bufferA,
        bufferB,
        bufferC,
        bufferD,
        common,
        sound,
        cubemap,
        fileList,
    });
};

export const parseInput = (input: input) => {
    const { channel, filepath, id, previewfilepath, published, sampler, type } =
        input;
    return { channel, filepath, id, previewfilepath, published, sampler, type };
};

export const useCommentProtector = () => {
    let commentMap = new Map<string, string>();

    const protectComments = (input: string): string => {
        let currentCounter = 0;

        const commentRegex = /\/\/.*|\/\*[\s\S]*?\*\//g; // /(\/\/.*$|\/\*[\s\S]*?\*\/)/gm

        const protectedText = input.replace(commentRegex, (match) => {
            const placeholder = `__COMMENT_${currentCounter++}__`;
            commentMap.set(placeholder, match);
            return placeholder;
        });

        return protectedText;
    };

    const restoreComments = (input: string): string => {
        let restoredText = input;

        commentMap.forEach((comment, placeholder) => {
            const placeholderRegex = new RegExp(placeholder, "g");
            restoredText = restoredText.replace(placeholderRegex, comment);
        });

        commentMap = new Map<string, string>();

        return restoredText;
    };

    return { protectComments, restoreComments };
};
