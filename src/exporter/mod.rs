use std::sync::Arc;

use itertools::Itertools;

use crate::compiler::Compiled;

pub fn export(compiled: Compiled) -> Result<String, ()> {
    let lists =
        [Arc::clone(&compiled.stdout), Arc::clone(&compiled.stack), Arc::clone(&compiled.args)];

    let lists = lists.into_iter().map(|list| format!(r#""{}": ["{}", []]"#, list.uuid, list.name));
    let lists = lists.collect_vec().join(",");

    let chains = compiled.chains.into_iter().map(|chain| chain.body.data(Some(&chain.root)));
    let chains = chains.collect::<Result<Vec<_>, _>>();
    let Ok(chains) = chains else { return Err(()) };
    let blocks = chains.into_iter().flat_map(|data| data.deps).collect_vec().join(",");

    let broadcasts = compiled
        .message_name_to_message
        .into_values()
        .map(|message| format!(r#""{}": "{}""#, message.uuid, message.name))
        .collect_vec()
        .join(",");

    let global_stage = format!(
        r#"
        {{
            "isStage": true,
            "name": "Stage",
            "variables": {{}},
            "lists": {{{}}},
            "broadcasts": {{{}}},
            "blocks": {{{}}},
            "comments": {{}},
            "currentCostume": 0,
            "costumes": [
                {{
                    "name": "backdrop1",
                    "dataFormat": "svg",
                    "assetId": "cd21514d0531fdffb22204e0ec5ed84a",
                    "md5ext": "cd21514d0531fdffb22204e0ec5ed84a.svg",
                    "rotationCenterX": 240,
                    "rotationCenterY": 180
                }}
            ],
            "sounds": [],
            "volume": 100,
            "layerOrder": 0,
            "tempo": 60,
            "videoTransparency": 50,
            "videoState": "on",
            "textToSpeechLanguage": null
        }}
    "#,
        lists, broadcasts, blocks
    );

    let stdout_monitor = format!(
        r#"
        {{
            "id": "{}",
            "mode": "list",
            "opcode": "data_listcontents",
            "params": {{ "LIST": "{}" }},
            "spriteName": null,
            "value": ["Hello world!", "1", "1"],
            "width": 404,
            "height": 299,
            "x": 5,
            "y": 5,
            "visible": true
        }}
    "#,
        compiled.stdout.uuid, compiled.stdout.name,
    );

    Ok(format!(
        r#"
        {{
            "targets": [{}],
            "monitors": [{}],
            "extensions": [],
            "meta": {{
                "semver": "3.0.0",
                "vm": "2.3.4",
                "agent": "ScratchASM/0.2.3"
            }}
        }}
    "#,
        global_stage, stdout_monitor
    ))
}
