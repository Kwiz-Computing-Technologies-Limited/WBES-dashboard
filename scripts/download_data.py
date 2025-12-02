#!/usr/bin/env python3
"""
Download WBES microdata from Google Drive
This script downloads the assets.zip file containing the real WBES microdata
"""

import os
import sys
import urllib.request
import shutil
from pathlib import Path

# Configuration
GDRIVE_FILE_ID = "1CFQ3djtgNlfKKB_2Ru7GHvkRGaKYkeuZ"
SCRIPT_DIR = Path(__file__).parent
PROJECT_ROOT = SCRIPT_DIR.parent
DATA_DIR = PROJECT_ROOT / "data"
ASSETS_ZIP = DATA_DIR / "assets.zip"

def download_file_from_google_drive(file_id, destination):
    """Download a file from Google Drive"""

    def get_confirm_token(response):
        for key, value in response.headers.items():
            if key.startswith('Set-Cookie'):
                if 'download_warning' in value:
                    return value.split('download_warning=')[1].split(';')[0]
        return None

    URL = "https://drive.google.com/uc?export=download"

    session = urllib.request.build_opener(urllib.request.HTTPCookieProcessor())

    response = session.open(f"{URL}&id={file_id}")
    token = get_confirm_token(response)

    if token:
        params = {'id': file_id, 'confirm': token}
        url = URL + "&" + urllib.parse.urlencode(params)
        response = session.open(url)

    CHUNK_SIZE = 32768
    with open(destination, "wb") as f:
        downloaded = 0
        while True:
            chunk = response.read(CHUNK_SIZE)
            if not chunk:
                break
            downloaded += len(chunk)
            f.write(chunk)
            # Print progress
            print(f"\rDownloaded: {downloaded / 1024 / 1024:.2f} MB", end='', flush=True)

    print()  # New line after download
    return downloaded

def main():
    print("=" * 60)
    print("WBES Dashboard - Data Download Script (Python)")
    print("=" * 60)
    print()
    print("This script will download the real WBES microdata")
    print("from Google Drive and set it up for the dashboard.")
    print()

    # Check if file exists
    if ASSETS_ZIP.exists():
        print(f"⚠️  WARNING: {ASSETS_ZIP} already exists")
        print()
        response = input("Do you want to overwrite it? (y/N): ").strip().lower()
        if response != 'y':
            print("Download cancelled.")
            return 0
        print(f"Removing existing {ASSETS_ZIP.name}...")
        ASSETS_ZIP.unlink()

    # Create data directory
    DATA_DIR.mkdir(parents=True, exist_ok=True)

    print()
    print("Downloading assets.zip from Google Drive...")
    print(f"File ID: {GDRIVE_FILE_ID}")
    print(f"Destination: {ASSETS_ZIP}")
    print()

    try:
        file_size = download_file_from_google_drive(GDRIVE_FILE_ID, ASSETS_ZIP)

        if not ASSETS_ZIP.exists():
            print("❌ Error: File not found after download")
            return 1

        actual_size = ASSETS_ZIP.stat().st_size

        if actual_size < 1_000_000:  # Less than 1MB is suspicious
            print(f"⚠️  Warning: File size is suspiciously small ({actual_size:,} bytes)")
            print("    This might be an error page instead of the actual file.")
            print("    Try downloading manually from the Google Drive link.")
            return 1

        print(f"✅ Download complete! File size: {actual_size / 1024 / 1024:.2f} MB")

        # Try to verify it's a ZIP file
        import zipfile
        try:
            with zipfile.ZipFile(ASSETS_ZIP, 'r') as zip_ref:
                print("✅ Valid ZIP file")
                print()
                print("ZIP file contents:")
                for info in zip_ref.filelist:
                    print(f"  - {info.filename} ({info.file_size:,} bytes)")
        except zipfile.BadZipFile:
            print("❌ Error: Downloaded file is not a valid ZIP archive")
            print("    Try downloading manually from the Google Drive link.")
            return 1

        print()
        print("=" * 60)
        print("✅ SUCCESS! Data download complete")
        print("=" * 60)
        print()
        print(f"The WBES microdata is now available at:")
        print(f"  {ASSETS_ZIP}")
        print()
        print("Next steps:")
        print("1. Start the dashboard: R -e 'rhino::app()'")
        print("2. The app will automatically load the real data")
        print("3. Check the logs for 'Found assets.zip - loading combined microdata'")
        print()

        return 0

    except Exception as e:
        print()
        print(f"❌ Download failed: {e}")
        print()
        print("Please download manually:")
        print(f"1. Visit: https://drive.google.com/file/d/{GDRIVE_FILE_ID}/view?usp=sharing")
        print("2. Click 'Download' button")
        print(f"3. Save as: {ASSETS_ZIP}")
        print()
        return 1

if __name__ == "__main__":
    sys.exit(main())
