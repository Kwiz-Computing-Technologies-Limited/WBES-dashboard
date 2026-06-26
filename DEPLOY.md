# Deploying the WBES dashboard (Google Cloud Run)

Live service: **https://wbes-dashboard-156769318979.us-central1.run.app**

| | |
|---|---|
| Project | `fit-asset-476910-t5` |
| Region | `us-central1` |
| Service | `wbes-dashboard` (Cloud Run, public / unauthenticated) |
| Image repo | `us-central1-docker.pkg.dev/fit-asset-476910-t5/kct-services/wbes-dashboard` |
| Resources | 2 GiB memory, 2 vCPU, CPU boost, scale 0–4 |
| Microdata bucket | `gs://fit-asset-476910-t5-wbes-data/processed.parquet` (private) |

## How data is delivered

The container has the aggregate artifacts **and** `processed.parquet` baked in
(`data/processed/`), so the app reads them locally — fast, no startup download.
`WBES_PROCESSED_URL=gs://fit-asset-476910-t5-wbes-data/processed.parquet` is set
as a **fallback**: if the local file is ever missing, the app downloads it from
that private bucket using the Cloud Run service account
(`156769318979-compute@developer.gserviceaccount.com`, granted
`roles/storage.objectViewer`). Resolution order is local → `gs://` → `https://`
→ run on aggregates only. See `app/logic/data_artifacts.R::resolve_processed_path()`.

## Redeploy (after code or data changes)

```bash
# 1. (only if the WBES data changed) rebuild artifacts from the raw .dta
Rscript scripts/build_data.R

# 2. (only if the microdata changed) refresh the bucket copy
gcloud storage cp data/processed/processed.parquet \
  gs://fit-asset-476910-t5-wbes-data/processed.parquet

# 3. build + deploy (no local Docker needed; uses Cloud Build)
PROJECT=fit-asset-476910-t5 REGION=us-central1 REPO=kct-services \
  ./scripts/deploy_cloudrun.sh
```

`scripts/deploy_cloudrun.sh` enables APIs, ensures the Artifact Registry repo,
builds with Cloud Build, and deploys. To also (re)set the data fallback env var,
add to the `gcloud run deploy` step:
`--set-env-vars WBES_PROCESSED_URL=gs://fit-asset-476910-t5-wbes-data/processed.parquet`.

## Notes

- **Cold starts:** with `min-instances=0` the service scales to zero; the first
  request after idle takes ~15 s (R loads packages + data). Set
  `--min-instances=1` to keep one warm instance (small always-on cost).
- **Cost:** Cloud Run bills per request/CPU-time and is ~free when idle; the
  image sits in Artifact Registry; the bucket holds ~6 MB. All minimal.
- **Never** put `data/assets.zip` (674 MB raw `.dta`) in the image — `.gcloudignore`
  excludes it. It is only needed locally to re-run `scripts/build_data.R`.
