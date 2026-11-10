"""Local-only git race tests. No production repository interaction."""
import os,subprocess,tempfile,unittest
from pathlib import Path
SCRIPT=Path(__file__).with_name('push_collected.sh').resolve()
def run(args,cwd,check=True,env=None):
 return subprocess.run(args,cwd=cwd,check=check,env=env,stdout=subprocess.PIPE,stderr=subprocess.STDOUT,text=True)
class GitTests(unittest.TestCase):
 def fixture(self,d):
  root=Path(d);remote=root/'remote.git';work=root/'work';other=root/'other'
  run(['git','init','--bare',str(remote)],root);run(['git','clone',str(remote),str(work)],root)
  for k,v in [('user.email','test@example.invalid'),('user.name','Test')]:run(['git','config',k,v],work)
  (work/'output.csv').write_text('initial\n');run(['git','add','.'],work);run(['git','commit','-m','initial'],work);run(['git','push','origin','HEAD:master'],work)
  run(['git','clone',str(remote),str(other)],root)
  for k,v in [('user.email','test@example.invalid'),('user.name','Test')]:run(['git','config',k,v],other)
  env=dict(os.environ,TARGET_BRANCH='master',RUNNER_TEMP=str(root/'temp'));return root,remote,work,other,env
 def test_normal_noop_and_missing(self):
  with tempfile.TemporaryDirectory() as d:
   root,remote,w,o,e=self.fixture(d)
   (w/'output.csv').write_text('initial\nnew\n')
   self.assertEqual(run(['bash',str(SCRIPT),'output.csv'],w,False,e).returncode,0)
   self.assertEqual(run(['bash',str(SCRIPT),'output.csv'],w,False,e).returncode,0)
   self.assertNotEqual(run(['bash',str(SCRIPT),'absent.csv'],w,False,e).returncode,0)
 def test_preexisting_remote_update(self):
  with tempfile.TemporaryDirectory() as d:
   root,remote,w,o,e=self.fixture(d)
   (o/'unrelated.txt').write_text('other\n');run(['git','add','.'],o);run(['git','commit','-m','other'],o);run(['git','push'],o)
   (w/'output.csv').write_text('initial\nnew\n')
   self.assertEqual(run(['bash',str(SCRIPT),'output.csv'],w,False,e).returncode,0)
   self.assertTrue((w/'unrelated.txt').exists())
 def test_push_race_then_retry(self):
  with tempfile.TemporaryDirectory() as d:
   root,remote,w,o,e=self.fixture(d)
   (o/'racing.txt').write_text('remote won first push\n');run(['git','add','.'],o);run(['git','commit','-m','race'],o)
   hook=w/'.git/hooks/pre-push'
   hook.write_text('#!/bin/sh\nif [ ! -e "'+str(root/'raced')+'" ]; then\n touch "'+str(root/'raced')+'"\n git -C "'+str(o)+'" push origin HEAD:master >/dev/null 2>&1\nfi\nexit 0\n')
   hook.chmod(0o755)
   (w/'output.csv').write_text('initial\nnew\n')
   result=run(['bash',str(SCRIPT),'output.csv'],w,False,e)
   self.assertEqual(result.returncode,0,result.stdout);self.assertTrue((w/'racing.txt').exists())
   self.assertIn('Push attempt 1 failed',result.stdout)
 def test_conflict_snapshot_survives(self):
  with tempfile.TemporaryDirectory() as d:
   root,remote,w,o,e=self.fixture(d)
   (o/'output.csv').write_text('conflicting remote\n');run(['git','add','.'],o);run(['git','commit','-m','other'],o);run(['git','push'],o)
   (w/'output.csv').write_text('local observation\n')
   result=run(['bash',str(SCRIPT),'output.csv'],w,False,e)
   self.assertNotEqual(result.returncode,0);self.assertEqual((root/'temp/tokenomics-preserved/output.csv').read_text(),'local observation\n')
   self.assertIn('not pushed',result.stdout)
 def test_persistent_push_failure_preserved(self):
  with tempfile.TemporaryDirectory() as d:
   root,remote,w,o,e=self.fixture(d)
   hook=remote/'hooks/pre-receive';hook.write_text('#!/bin/sh\nexit 1\n');hook.chmod(0o755)
   (w/'output.csv').write_text('local observation\n')
   result=run(['bash',str(SCRIPT),'output.csv'],w,False,e)
   self.assertNotEqual(result.returncode,0);self.assertEqual((root/'temp/tokenomics-preserved/output.csv').read_text(),'local observation\n')
   self.assertIn('not confirmed saved',result.stdout)
if __name__=='__main__':unittest.main()
