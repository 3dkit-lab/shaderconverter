import Handlebars from "handlebars";
import { generateGuid, getInternalFile } from "@/utils";
import {
    BaseMeta,
    NativeFormatImporter,
    ShaderImporter,
    TextureImporter,
} from "./baseMeta";
import { UnityBaseMaterial } from "./baseMaterial";
import { toUnityShader } from "./converter";
import { shader, internalFile } from "@/typing";
import { texture2D } from "@/constant";

const generateShaderMeta = (guid: string, internalList: internalFile[]) => {
    const template = Handlebars.compile(BaseMeta);
    let defaultTextures = internalList.length > 0 ? "" : " []";
    for (const item of internalList) {
        const { guid, channel } = item;
        const texName = texture2D[channel]; // todo: with type
        const file = `\t\t- ${texName}: {fileID: 2800000, guid: ${guid}, type: 3}`;
        defaultTextures += `\n${file}`;
    }
    console.log(defaultTextures);
    return template({
        Importer: Handlebars.compile(ShaderImporter)({ defaultTextures }),
        guid,
    });
};

const generateImageMeta = (guid: string) => {
    const template = Handlebars.compile(BaseMeta);
    return template({
        Importer: TextureImporter,
        guid,
    });
};

const generateMaterialMeta = (guid: string) => {
    const template = Handlebars.compile(BaseMeta);
    return template({
        guid,
        Importer: NativeFormatImporter,
    });
};

const generateMaterial = (
    name: string,
    guid: string,
    internalList: internalFile[],
) => {
    const template = Handlebars.compile(UnityBaseMaterial);
    let m_TexEnvs = internalList.length > 0 ? "" : " []";
    for (const item of internalList) {
        const { guid, channel } = item;
        const texName = texture2D[channel]; // todo: with type
        const tex = `\t\t- ${texName}:
        m_Texture: {fileID: 2800000, guid: ${guid}, type: 3}
        m_Scale: {x: 1, y: 1}
        m_Offset: {x: 0, y: 0}`;
        m_TexEnvs += `\n${tex}`;
    }
    return template({ name, guid, m_TexEnvs });
};

export const generate = (name: string, shader: shader) => {
    const files = [],
        internalList: internalFile[] = [];
    const { fileList } = shader.data;
    for (const key in fileList) {
        const input = fileList[key];
        const { type, id, channel } = input;
        const guid = generateGuid();
        const { name, url, ext } = getInternalFile(id);
        internalList.push({ type, guid, name, url, ext, channel });
        files.push({ name: `${name}`, url });
        // todo: with type
        files.push({ name: `${name}.meta`, content: generateImageMeta(guid) });
    }
    const shaderFileGuid = generateGuid();
    const shaderMetaFile = generateShaderMeta(shaderFileGuid, internalList);
    files.push({ name: `${name}.shader.meta`, content: shaderMetaFile });
    const shaderFile = toUnityShader(name, shader);
    files.push({ name: `${name}.shader`, content: shaderFile });
    const materialFileGuid = generateGuid();
    const materialMetaFile = generateMaterialMeta(materialFileGuid);
    files.push({ name: `${name}.mat.meta`, content: materialMetaFile });
    const materialFile = generateMaterial(name, shaderFileGuid, internalList);
    files.push({ name: `${name}.mat`, content: materialFile });
    return files;
};
