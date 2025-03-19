type renderpassType =
    | "image"
    | "buffer"
    | "common"
    | "keyboard"
    | "mouse"
    | "sound"
    | "cubemap";

export interface shadertoy {
    ver: string;
    info: info;
    renderpass: renderpass[];
}

export interface info {
    id: string;
    date: string;
    viewed: number;
    name: string;
    username: string;
    description: string;
    likes: number;
    published: number;
    flags: number;
    usePreview: number;
    tags: string[];
    hasliked: boolean;
    parentid: string;
    parentname: string;
}

export interface renderpass {
    code: string;
    description: string;
    inputs: input[];
    name: string;
    outputs: output[];
    type: renderpassType;
}

export interface input {
    channel: number;
    filepath: string;
    id: string;
    previewfilepath: string;
    published: number;
    sampler: sampler;
    type: inputType;
}

type inputType =
    | "texture"
    | "volume"
    | "buffer"
    | "cubemap"
    | "video"
    | "music";

export interface output {
    id: string;
    channel: number;
}

interface sampler {
    filter: string;
    wrap: string;
    vflip: string;
    srgb: string;
    internal: string;
}

export interface shader {
    name: string;
    data: renderpassData;
    multipass: boolean;
}

export interface renderpassData {
    [key: string]: renderpass;
}
