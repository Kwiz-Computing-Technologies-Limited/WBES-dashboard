#!/usr/bin/env bash
# scripts/deploy_cloudrun.sh
# Build the WBES dashboard image with Cloud Build and deploy it to Cloud Run.
# No local Docker daemon required.
#
# Prereqs:
#   gcloud auth login          # interactive (browser) — refresh expired creds
#   gcloud config set project <PROJECT>
#
# Usage:
#   PROJECT=fit-asset-476910-t5 REGION=us-central1 ./scripts/deploy_cloudrun.sh
set -euo pipefail

PROJECT="${PROJECT:-$(gcloud config get-value project 2>/dev/null)}"
REGION="${REGION:-us-central1}"
SERVICE="${SERVICE:-wbes-dashboard}"
REPO="${REPO:-kct-services}"
IMAGE="${REGION}-docker.pkg.dev/${PROJECT}/${REPO}/${SERVICE}:$(date +%Y%m%d-%H%M%S)"

echo "Project=${PROJECT}  Region=${REGION}  Service=${SERVICE}"
echo "Image=${IMAGE}"

echo "==> Enabling required APIs (idempotent)"
gcloud services enable \
  run.googleapis.com \
  cloudbuild.googleapis.com \
  artifactregistry.googleapis.com \
  storage.googleapis.com \
  --project "${PROJECT}"

echo "==> Ensuring Artifact Registry repo '${REPO}' exists"
gcloud artifacts repositories describe "${REPO}" --location "${REGION}" --project "${PROJECT}" >/dev/null 2>&1 || \
  gcloud artifacts repositories create "${REPO}" \
    --repository-format=docker --location "${REGION}" --project "${PROJECT}" \
    --description "Container images"

echo "==> Building image with Cloud Build (timeout 30m)"
gcloud builds submit --tag "${IMAGE}" --timeout=1800s --project "${PROJECT}"

echo "==> Deploying to Cloud Run"
gcloud run deploy "${SERVICE}" \
  --image "${IMAGE}" \
  --region "${REGION}" \
  --project "${PROJECT}" \
  --platform managed \
  --allow-unauthenticated \
  --port 8080 \
  --memory 2Gi \
  --cpu 2 \
  --cpu-boost \
  --no-cpu-throttling \
  --timeout 300 \
  --concurrency 60 \
  --min-instances 1 \
  --max-instances 4 \
  --set-env-vars "WBES_PROCESSED_URL=gs://${PROJECT}-wbes-data/processed.parquet"

echo "==> Done. Service URL:"
gcloud run services describe "${SERVICE}" --region "${REGION}" --project "${PROJECT}" \
  --format='value(status.url)'
