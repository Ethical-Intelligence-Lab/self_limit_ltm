import os
import glob
from pdb import set_trace

input_path = '/Users/julian/Dropbox (Personal)/Research/Intuition/single_identity/e15_memory_alternatives_hard/images/'
#input_folders = glob.glob(input_path)

#os.chdir(input_path)
#for i,j,y in os.walk('.'):
#    print(i)
    
#set_trace()

#for folder in input_folders:
#    images = glob.glob(folder + '*.jpg')
#    for image in images:
#        video_list = []
#        set_trace()
#        currImage = re.split('/', video)[8]
#        currFolder = re.split('/',video)[7]

#root_dir = 'Users/julian/Dropbox (Personal)/Research/Intuition/single_identity/e5_LTM/images/'
for dir, subdirs, files in os.walk(input_path):
    counter = 1
    for f in files:
        if f != '.DS_Store':
            cat = dir.split('/')[9]
            f_new = cat + str(counter) + '.png'
            os.rename(os.path.join(dir, f), os.path.join(dir, f_new))
            counter += 1
