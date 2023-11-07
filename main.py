import pandas as pd
from tkinter import Tk
from tkinter.filedialog import askopenfilename
from tkinter import messagebox

# this will hide the main tkinter window
Tk().withdraw()

# this will show a pop-up window with a message
messagebox.showinfo("Instructions","Please navigate to the location of the PUF and double click on the file, or click open.")

# this opens the file explorer and store the selected file path into 'file_path'
file_path = askopenfilename() 

# read in data from csv
puf = pd.read_csv(file_path)