"""Check the 'Shimadzu' single quadrupole centroid decoder against msconvert.

Usage: python3 dev/validate_sq_msconvert.py <run.lcd> <run.mzML>

The mzML is an msconvert (ProteoWizard 3.0.26198) export of the same run,
without peak picking. Each scan's centroids are decoded from `Mass Raw Data`
(m/z as a uint16 / 20, then an intensity of 1-4 bytes) and compared with the
mzML spectrum: polarity, retention time, point count, m/z and intensities, and
TIC. It also checks whether msconvert's point order is a stable sort of the
stored points by m/z, which is the order `read_sz_sq` returns. It prints a
count of each outcome and the first scans whose stored order differs.

On MTBLS1960 `E1.lcd` and `st.lcd` all 4,800 scans agree on every field;
434 and 402 scans store their points out of m/z order. It handles centroid
scans only, not the profile scans checked by `dev/validate_sq_profile.py`.
"""
import olefile,sys,struct,numpy as np,base64,zlib,collections
import xml.etree.ElementTree as ET
lcd,mzml=sys.argv[1],sys.argv[2]
o=olefile.OleFileIO(lcd)
g=lambda n:o.openstream("Mass Raw Data/"+n).read()
ms=g("MS Raw Data"); off=np.append(np.frombuffer(g("Spectrum Index"),"<u4"),len(ms))
raw=[]
for i in range(len(off)-1):
  u=struct.unpack("<32H",ms[off[i]:off[i]+64]); w,n=u[26],u[27]
  rec=np.frombuffer(ms[off[i]+64:off[i+1]],np.uint8).reshape(n,2+w)
  mz=(rec[:,0].astype(int)+256*rec[:,1])/20
  it=sum(rec[:,2+k].astype(np.int64)<<(8*k) for k in range(w))
  rt=struct.unpack_from("<I",ms,off[i]+4)[0]
  raw.append((mz,it,"negative" if u[22]&4 else "positive",rt))
ns="{http://psi.hupo.org/ms/mzml}"
c=collections.Counter(); k=0; ex=[]
for ev,el in ET.iterparse(mzml):
  if el.tag!=ns+"spectrum": continue
  names={p.get("name"):p.get("value") for p in el.iter(ns+"cvParam")}
  pol="positive" if "positive scan" in names else "negative" if "negative scan" in names else "?"
  arrs={}
  for b in el.iter(ns+"binaryDataArray"):
    pn={p.get("name") for p in b.iter(ns+"cvParam")}
    d=base64.b64decode(b.find(ns+"binary").text or "")
    if "zlib compression" in pn: d=zlib.decompress(d)
    a=np.frombuffer(d,"<f8" if "64-bit float" in pn else "<f4")
    arrs["mz" if "m/z array" in pn else "int"]=a
  mz,it,rpol,rt=raw[k]
  c["pol "+("ok" if pol==rpol else f"{pol}!={rpol}")]+=1
  c["rt "+("ok" if abs(float(names["scan start time"])-rt/1000)<1e-6 else "diff")]+=1
  c["n "+("ok" if len(arrs["mz"])==len(mz) else "diff")]+=1
  if len(arrs["mz"])==len(mz):
    same_order=np.allclose(arrs["mz"],mz,atol=1e-9) and np.array_equal(arrs["int"],it)
    o_=np.lexsort((it,mz)); o2=np.lexsort((arrs["int"],arrs["mz"]))
    same_set=np.allclose(arrs["mz"][o2],mz[o_],atol=1e-9) and np.array_equal(arrs["int"][o2],it[o_])
    st=np.argsort(mz,kind="stable"); c["pwiz sorted "+str(bool(np.all(np.diff(arrs["mz"])>=0)))]+=1; c["= stable sort of stored "+str(bool(np.allclose(arrs["mz"],mz[st]) and np.array_equal(arrs["int"],it[st])))]+=1
    c["stored order "+str(same_order)]+=1; c["same points "+str(same_set)]+=1
    if not same_order and len(ex)<2: ex.append((k,arrs["mz"][:4],arrs["int"][:4],mz[:4],it[:4]))
    if not same_set and len(ex)<4: ex.append(("SET",k,np.max(np.abs(arrs["mz"][o2]-mz[o_]))))
  c["tic "+("ok" if abs(float(names["total ion current"])-it.sum())<=it.sum()*1e-4 else "diff")]+=1
  k+=1; el.clear()
print(lcd.split("/")[-1], k, dict(c))
for e in ex: print(e)
