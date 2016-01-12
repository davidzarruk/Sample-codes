import requests
import unicodedata
import csv
import numpy
import os

os.chdir("C:\\Users\\david\\Dropbox\\Documents\\Doctorado\\Research\\Didris\\SPADIES")
url="http://spadies.mineducacion.gov.co/spadies/spadies/consultas"

x1 = numpy.array([10,12,22,16,17,6,14,13,20,21,69,24,25,26,34,35,36,30,32,31,33,71, 64])
x2 = numpy.array([63])


for ano in numpy.nditer(x2):
	for var2 in numpy.nditer(x1):
		with open('universities.csv', newline='') as csvfile:
			spamreader = csv.reader(csvfile, delimiter=' ', quotechar='|')
			for institut in spamreader:
				name = 'CSV\\var_'+str(var2)+'_ano_'+str(ano)+'_univ_'+str(institut[0])+'.csv'

				types = 'w'
				initial = 0
				c='4'

				diferenciados=str(ano)+','+str(var2)+','
				fil='!2--,'+str(institut[0])
				modo='0'
				stri='c='+c+'&'+'diferenciados='+diferenciados+'&'+'fil='+fil+'&'+'modo='+modo

				r = requests.post(url, data=stri)
				sol = requests.post(url, data=stri)
				JSONDATA=sol.json()
				print(stri)

				if JSONDATA != 'La consulta no arrojó datos.':
					rows = len(JSONDATA[3]['valores'])
					univ = int(institut[0])*numpy.ones((rows-initial,1))
					A = numpy.array(JSONDATA[3]['valores'][initial:rows])

					A = numpy.hstack((univ,A))

					with open(name, types, newline='') as fp:
						a = csv.writer(fp, delimiter=',')
						a.writerows(A)

# Lo siguiente es solamente para primer semestre
x1 = numpy.array([20, 21,30,32,31,33])
x1 = numpy.array([20])

for ano in numpy.nditer(x2):
	for var2 in numpy.nditer(x1):
		with open('universities.csv', newline='') as csvfile:
			spamreader = csv.reader(csvfile, delimiter=' ', quotechar='|')
			for institut in spamreader:
				name = 'CSV\\var_'+str(var2)+'_ano_'+str(ano)+'_univ_'+str(institut[0])+'_first.csv'

				types = 'w'
				initial = 0
				c='4'

				diferenciados=str(ano)+','+str(var2)+','
				fil='!2--,'+str(institut[0])+'!64--,1'
				modo='0'
				stri='c='+c+'&'+'diferenciados='+diferenciados+'&'+'fil='+fil+'&'+'modo='+modo

				r = requests.post(url, data=stri)
				sol = requests.post(url, data=stri)
				JSONDATA=sol.json()
				print(stri)

				if JSONDATA != 'La consulta no arrojó datos.':
					rows = len(JSONDATA[3]['valores'])
					univ = int(institut[0])*numpy.ones((rows-initial,1))
					A = numpy.array(JSONDATA[3]['valores'][initial:rows])

					A = numpy.hstack((univ,A))

					with open(name, types, newline='') as fp:
						a = csv.writer(fp, delimiter=',')
						a.writerows(A)



x1 = numpy.arange(10, 60, 1)

for ano in numpy.nditer(x2):
	for var2 in numpy.nditer(x1):
		name = 'CSV\\var_'+str(var2)+'_ano_'+str(ano)+'_univ_'+str(1101)+'_first.csv'

		types = 'w'
		initial = 0
		c='4'

		diferenciados=str(ano)+','+str(var2)+','
		fil='!2--,'+str(1101)+'!64--,1'
		modo='0'
		stri='c='+c+'&'+'diferenciados='+diferenciados+'&'+'fil='+fil+'&'+'modo='+modo
		try:
			r = requests.post(url, data=stri)
			sol = requests.post(url, data=stri)
			JSONDATA=sol.json()
			print(stri)

			if JSONDATA != 'La consulta no arrojó datos.':
				rows = len(JSONDATA[3]['valores'])
				univ = int(1101)*numpy.ones((rows-initial,1))
				A = numpy.array(JSONDATA[3]['valores'][initial:rows])

				A = numpy.hstack((univ,A))

				with open(name, types, newline='') as fp:
					a = csv.writer(fp, delimiter=',')
					a.writerows(A)
		except:
			pass