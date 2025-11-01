# vxmb REST API

Base URL `/api/v1`. All bodies JSON. Errors return HTTP 500 with empty body.

`eval_id` identifies the namespace created for a client. `engine_id` and `speaker_id` are zero-based indexes. `task_id` comes from the queue.

## POST `/api/v1/evaluations`
Create a namespace (`vxmb:new/0`).  
201 body: `{ "eval_id": "...", "config": {...} }`

## POST `/api/v1/evaluations/{eval_id}/tasks`
Fetch ready tasks (`vxmb:fetch_tasks/1`).  
200 body: `{ "tasks": [{ "task_id": "...", "speaker_id": 0 }] }`  
Blocks internally until tasks are available.
Returns an empty array once the queue is exhausted.

## PUT `/api/v1/evaluations/{eval_id}/engines/{engine_id}/speaker`
Set engine speaker (`vxmb:set_engine_speaker/3`).  
Request body: `{ "speaker_id": 0 }`  
202 body: `{}`  
Blocks internally while applying overhead.

## POST `/api/v1/evaluations/{eval_id}/engines/{engine_id}/synthesis`
Complete a task (`vxmb:synthesis/4`).  
Request body: `{ "speaker_id": 0, "task_id": "..." }`  
202 body: `{}`  
Blocks internally for task latency.

## GET `/api/v1/evaluations/{eval_id}/metrics`
Get score (`vxmb:evaluate/1`).  
200 body: `{ "score": 12345.67 }`

## GET `/api/v1/config`
Read defaults (`vxmb:config/0`).  
200 body: `{ "task_count": 100000, "engine_count": 4, "speaker_count": 255 }`
