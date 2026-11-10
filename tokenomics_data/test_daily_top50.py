import importlib.util,json,tempfile,unittest,csv
from datetime import datetime, timezone
from pathlib import Path
spec=importlib.util.spec_from_file_location('daily',Path(__file__).with_name('fetch_daily_top50.py'));m=importlib.util.module_from_spec(spec);spec.loader.exec_module(m)
class Tests(unittest.TestCase):
 def payload(self):
  return {'data':[dict(date='2026-11-09 00:00:00',model_permaslug=f'p/m{i}',variant=v,variant_permaslug=f'p/m{i}'+('' if v=='standard' else ':free'),total_prompt_tokens=i*10,total_completion_tokens=i,count=i+1) for i in range(1,61) for v in ['standard','free']]}
 def test_base_and_variants(self):
  p=self.payload();a=m.select(p,'2026-11-09','base_model');b=m.select(p,'2026-11-09','variant')
  self.assertEqual(len(a),50);self.assertEqual(a[0],('p/m60',[1200,120,122,2]));self.assertEqual(a[-1][0],'p/m11');self.assertEqual(len(b),50);self.assertEqual(len(set(k.split(':')[0] for k,v in b)),25)
 def test_bad_inputs(self):
  for key,value in [('date','2026-11-08'),('total_prompt_tokens',None),('count',-1),('total_completion_tokens',True),('variant',None)]:
   p=self.payload();p['data'][0][key]=value
   with self.assertRaises(ValueError):m.select(p,'2026-11-09','base_model')
  p=self.payload();p['data'].append(p['data'][0]);
  with self.assertRaises(ValueError):m.select(p,'2026-11-09','base_model')
  p=self.payload();p['data']=p['data'][:50]
  with self.assertRaises(ValueError):m.select(p,'2026-11-09','base_model')
 def test_atomic_idempotent_no_backfill(self):
  with tempfile.TemporaryDirectory() as d:
   out=Path(d)/'x.csv';raw=Path(d)/'raw';now=datetime(2026,11,10,13,tzinfo=timezone.utc)
   fetch=lambda:json.dumps(self.payload()).encode()
   self.assertEqual(m.collect(out,raw,'base_model',fetch,now),50)
   before=out.read_bytes();self.assertEqual(m.collect(out,raw,'base_model',lambda:1/0,now),0);self.assertEqual(before,out.read_bytes())
   self.assertEqual(len(list(raw.iterdir())),1)
   # Existing complete day cannot silently change ranking unit.
   with self.assertRaises(ValueError):m.collect(out,raw,'variant',fetch,now)
   with out.open() as f: rows=list(csv.DictReader(f))
   rows.pop()
   with out.open('w') as f:
    w=csv.DictWriter(f,fieldnames=m.FIELDS);w.writeheader();w.writerows(rows)
   with self.assertRaises(ValueError):m.collect(out,raw,'base_model',fetch,now)
if __name__=='__main__':unittest.main()
