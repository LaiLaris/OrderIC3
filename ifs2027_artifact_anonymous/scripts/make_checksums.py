"""Hash frozen artifact content; exclude reproducible outputs and build caches."""
from pathlib import Path
import hashlib
R=Path(__file__).resolve().parents[1]
def digest(p):
 h=hashlib.sha256()
 with p.open('rb') as f:
  for b in iter(lambda:f.read(1024*1024),b''):h.update(b)
 return h.hexdigest()
files=[]
for p in sorted(R.rglob('*')):
 rel=p.relative_to(R)
 if not p.is_file() or p.name=='checksums.sha256':continue
 if any(x in rel.parts for x in ['_build','__pycache__']):continue
 if rel.parts[0] in ['generated','figures','replays']:continue
 files.append(p)
(R/'checksums.sha256').write_text(''.join(digest(p)+'  '+str(p.relative_to(R))+'\n' for p in files))
print('Hashed',len(files),'files; generated outputs and build caches are excluded.')
