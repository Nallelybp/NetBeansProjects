/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package lme4GS;

import java.lang.management.ManagementFactory;
import java.util.Scanner;
import javax.swing.filechooser.FileFilter;
import javax.swing.plaf.metal.MetalLookAndFeel;
import javax.swing.plaf.metal.OceanTheme;
import java.awt.Color;
import java.awt.Desktop;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Toolkit;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.DefaultComboBoxModel;
import javax.swing.DefaultListModel;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JTable;
import javax.swing.ListModel;
import javax.swing.SwingWorker;
import javax.swing.UIManager;
import javax.swing.filechooser.FileNameExtensionFilter;
import javax.swing.plaf.basic.BasicProgressBarUI;
import javax.swing.table.DefaultTableModel;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreePath;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.rosuda.JRI.REXP;
import org.rosuda.JRI.Rengine;



/**
 *
 * @author FRHUERTA
 */
public class BGLR extends javax.swing.JFrame {
    private DefaultTableModel model;
    private DefaultTableModel modelMarkers;
    private DefaultTableModel modelPed;
    private DefaultTableModel modelEC;
    private DefaultTableModel modelCV;
    private String filedir="";
    String[] menu3;
    DefaultListModel modelList = new DefaultListModel();
    private ProgressInfinite taskInfinite;
    About about = null;
    
    
    MarkersView markerview = null;
    RelationshipView pedview = null;
    ECView ECview = null;
    CVView CVview = null;
    Rengine re = Rengine.getMainEngine();
    int wasstop=1;
    int Examples=0;
    int ExamplesMark=0;
    int ExamplesPed=0;
    int ExamplesEC=0;
    int ExamplesCVSets=0;
    int worktodo=0;
    String mainPath;
    String pathname;
    String filename;
    String actualOutputDir;
    
       
    /**
     * Creates new form BGLR
     * @throws java.io.IOException
     */
    public BGLR(){
        super("Linear Mixed-Effects Models for Genomic Selection (lme4GS)");
        this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) { 
                int confirmed = JOptionPane.showConfirmDialog(null, 
                        "Are you sure you want to quit?", "Confirm Quit",
                        JOptionPane.YES_NO_OPTION); 
                //Close if user confirmed 
                if (confirmed == JOptionPane.YES_OPTION) {                             
                    //Close frame 
                    re.eval("rm(list=ls())");
                    dispose(); 
                } else{
                    setDefaultCloseOperation(javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE);
                }
            }
        });
        if(re == null)
            re = new Rengine(new String[] {"--vanilla"},false,null);
        initComponents();
        setSize(new Dimension(1220,680));
        //setExtendedState(java.awt.Frame.MAXIMIZED_BOTH);
        setLocationRelativeTo( null );
        setVisible(true);
        ResultsPanel.setVisible(false);
        Methodpanel.setVisible(false);
        SitesTo.setVisible(false);
        ResponsesPanel.setVisible(false);
        Variablespanel.setVisible(false);
        jScrollPane1.setVisible(false);
        PanelBar.setVisible(false);
        Ran_Eff.setVisible(false);
        Fix_Eff.setVisible(false);
        BigPanel.setVisible(false);
               
        CVModule.addWindowListener(closeWindow);
        CVModule.setLocationRelativeTo(this);
        CVModule.setBounds(132, 132, 422, 280);
        Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
        screenSize = Toolkit.getDefaultToolkit().getScreenSize();
        int xW = (screenSize.width/2) - (CVModule.getWidth()/2);
        int yW = (screenSize.height/2) - (CVModule.getHeight()/2);
        CVModule.setLocation(xW,yW);
        BigPanelCV.setPreferredSize(new Dimension(416, 150));
        CVpanel.setPreferredSize(new Dimension(394, 190));
        CVbutton.setVisible(false);
        NumberCV.setVisible(false);
        NumberCVLabel.setVisible(false);
        TestingSize.setVisible(false);
        TestingSizeLabel.setVisible(false);
        CVSetname.setVisible(false);
        LoadCVSets.setVisible(false);
        SeeCVSets.setVisible(false);
        
        long memorySizeAvailable = ((com.sun.management.OperatingSystemMXBean) ManagementFactory.getOperatingSystemMXBean()).getFreePhysicalMemorySize()/(1024*1024);
        re.assign("memSize", Long.toString(memorySizeAvailable));
        re.eval("error <- try(memory.limit(as.numeric(as.character(memSize))))");
        re.eval("errorRead <- ifelse(inherits(error,'try-error'),99,0)");
        REXP outputvalue = re.eval("errorRead");
        if(outputvalue.asDouble() == 99){
            //outputvalue = re.eval("error");
            //String messageS = outputvalue.asString();                 
            JOptionPane.showMessageDialog(this, "\n\n It looks like your system is using Java 32 bit, and it does not allow you "
                                                + "\n to use all available memory in your system."
                                              + "\n\n Actually it uses 4 GB of your free memory, if you want to improve this and "
                                                + "\n use all available memory, you must change Java 32 bit by Java 64 bit."
                                              + "\n\n For more information you can go to 'README_BGLR-R.txt'");
        }
    }
    
    private static class MyProgressUI extends BasicProgressBarUI {
        private Rectangle r = new Rectangle();
        @Override
        protected void paintIndeterminate(Graphics g, JComponent c) {
            Graphics2D g2d = (Graphics2D) g;
            g2d.setRenderingHint(
                RenderingHints.KEY_ANTIALIASING,
                RenderingHints.VALUE_ANTIALIAS_ON);
            r = getBox(r);
            g.setColor(progressBar.getForeground());
            g.fillRect(r.x, r.y, r.width, r.height);
        }
    }
    
    class ProgressInfinite extends SwingWorker<Void,Void>{
        @Override
        public Void doInBackground() throws IOException{
            PanelBar.setVisible(true);
            UIManager.put("ProgressBar.repaintInterval", 20);
            UIManager.put("ProgressBar.cycleTime", 2000);
            Pbarra.setUI(new MyProgressUI());
            Color colr = new Color(102,102,0);
            Pbarra.setForeground(colr);
            Pbarra.setIndeterminate(true);
            task();
            return null;
            //throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
        }
        
        @Override
        public void done(){
            Pbarra.setIndeterminate(false);
            PanelBar.setVisible(false);
        }
    }
    
    @SuppressWarnings("unchecked")
    DefaultMutableTreeNode addNodes(DefaultMutableTreeNode curTop, File dir) {
        String curPath = dir.getPath();
        DefaultMutableTreeNode curDir = new DefaultMutableTreeNode(curPath);
        if (curTop != null) { // should only be null at root
            curTop.add(curDir);
        }
        Vector ol = new Vector();
        String[] tmp = dir.list();
        for (int i = 0; i < tmp.length; i++)
            ol.addElement(tmp[i]);
        Collections.sort(ol, String.CASE_INSENSITIVE_ORDER);
        File f;
        Vector files = new Vector();
        // Make two passes, one for Dirs and one for Files. This is #1.
        for (int i = 0; i < ol.size(); i++) {
            String thisObject = (String) ol.elementAt(i);
            String newPath;
            if (curPath.equals("."))
                newPath = thisObject;
            else
                newPath = curPath + File.separator + thisObject;
            if ((f = new File(newPath)).isDirectory())
                addNodes(curDir, f);
            else
                files.addElement(thisObject);
        }
        // Pass two: for files.
        for (int fnum = 0; fnum < files.size(); fnum++)
            curDir.add(new DefaultMutableTreeNode(files.elementAt(fnum)));
        return curDir;
    }
    
    private static WindowListener closeWindow = new WindowAdapter() {
        public void windowClosing(WindowEvent e) {
            e.getWindow().dispose();
        }
    };
    
    @SuppressWarnings("unchecked")
    public void CargarDatos(){
        Analyzing.setVisible(false);
        Loading.setVisible(true);
        Recoding.setVisible(false);
        Imputing.setVisible(false);
        int result, result2, result21;
        String[] colNames=null;
        result = filedir.indexOf( ".txt" );
        result2 = filedir.indexOf( ".csv" );
        result21 = filedir.indexOf( ".CSV" );
        if(result != -1 || result2 != -1 || result21 != -1){
            //txtDisplay.setText( file_name );
            Scanner scanner = null;
            try {
                scanner = new Scanner(new File(filedir));
            } catch (FileNotFoundException ex) {
                Logger.getLogger(BGLR.class.getName()).log(Level.SEVERE, null, ex);
            }
            String line = scanner.nextLine();
            if(result != -1){
                colNames = line.split("\t");
                model = new DefaultTableModel(colNames, 0);
                String[] array;
                while (scanner.hasNextLine()) {
                    line = scanner.nextLine();
                    array = line.split("\t");
                    Object[] data = new Object[array.length];
                    System.arraycopy(array, 0, data, 0, array.length);
                    model.addRow(data);
                }
            }else{
                colNames = line.split(",");
                model = new DefaultTableModel(colNames, 0);
                String[] array;
                while (scanner.hasNextLine()) {
                    line = scanner.nextLine();
                    if(line.contains(","))
                    array = line.split(",");
                    else
                    array = line.split("\t");
                    Object[] data = new Object[array.length];
                    System.arraycopy(array, 0, data, 0, array.length);
                    model.addRow(data);
                }
            }
            PanelMenus.setAutoResizeMode( JTable.AUTO_RESIZE_OFF );
            PanelMenus.setModel(model);
            scanner.close();
        }else{
            FileInputStream file2 = null;
            try {
            // try{
                file2 = new FileInputStream(new File(filedir));
                //Create Workbook instance holding reference to .xlsx file
                XSSFWorkbook workbook = new XSSFWorkbook(file2);
                //Get first/desired sheet from the workbook
                XSSFSheet sheet = workbook.getSheetAt(0);
                //Iterate through each rows one by one
                int i=0;
                Iterator<Row> rowIterator = sheet.iterator();
                Row row = rowIterator.next();
                Object[] data0 = new Object[row.getPhysicalNumberOfCells()];
                Iterator<Cell> cellIterator = row.cellIterator();
                while (cellIterator.hasNext()){
                    Cell cell = cellIterator.next();
                    data0[i] = cell.getStringCellValue();
                    i++;
                }
                String line = Arrays.toString(data0);
                String line1 = line.split("\\[")[1];
                String line2 = line1.split("\\]")[0];
                colNames = line2.split(", ");
                model = new DefaultTableModel(colNames, 0);
                while (rowIterator.hasNext()){
                    row = rowIterator.next();
                    Object[] data = new Object[row.getPhysicalNumberOfCells()];
                    //For each row, iterate through all the columns
                    Iterator<Cell> cellIterator2 = row.cellIterator();
                    i=0;
                    while (cellIterator2.hasNext()){
                        Cell cell = cellIterator2.next();
                        //Check the cell type and format accordingly
                        switch (cell.getCellType()){
                            case Cell.CELL_TYPE_NUMERIC:
                            data[i] = cell.getNumericCellValue();
                            break;
                            case Cell.CELL_TYPE_STRING:
                            data[i] = cell.getStringCellValue();
                            break;
                        }
                        i++;
                    }
                    model.addRow(data);
                }
                PanelMenus.setAutoResizeMode( JTable.AUTO_RESIZE_OFF );
                PanelMenus.setModel(model);
            } catch (FileNotFoundException ex) {
                Logger.getLogger(BGLR.class.getName()).log(Level.SEVERE, null, ex);
            } catch (IOException ex) {
                Logger.getLogger(BGLR.class.getName()).log(Level.SEVERE, null, ex);
            } finally {
                try {
                    file2.close();
                } catch (IOException ex) {
                    Logger.getLogger(BGLR.class.getName()).log(Level.SEVERE, null, ex);
                }
            }
        }
        int nitems = colNames.length;
        String[] menu1 = new String[nitems+1];
        menu1[0]="Choose variable";
        System.arraycopy(colNames, 0, menu1, 1, nitems);
        String[] menu2 = new String[nitems+2];
        menu2[0]="Choose variable";
        //menu2[1]="One site";
        System.arraycopy(colNames, 0, menu2, 1, nitems);

        //RepBox.setModel(new DefaultComboBoxModel(menu1));
        //BlkBox.setModel(new DefaultComboBoxModel(menu1));
        GenotypeBox.setModel(new DefaultComboBoxModel(menu1));
        menu3 = new String[nitems];
        System.arraycopy(colNames, 0, menu3, 0, nitems);
        for(int i=0; i < nitems; i++) {
            modelList.add(i, menu3[i]);
        }
        ListVariables.setModel(modelList);
        ListRandom.setModel(modelList);
        ListFixed.setModel(modelList);
        String[] parts;
        parts = filedir.split(":?\\\\");
        int s = parts.length;
        filename = parts[s-1];
        String[] parts2 = filedir.split(filename);
        pathname = parts2[0];
        re.eval("rm(list=ls())");
        re.assign("path",pathname);
        re.assign("archivo",filename);
        //String mainPath = System.getProperty("user.dir");
        String[] partsPath;
        partsPath = mainPath.split(":?\\\\");
        int sPath = partsPath.length;
        String filenamelib = partsPath[sPath-1];
        String[] partsPath2 = mainPath.split(filenamelib);
        String pathlib = partsPath2[0];
        String libPath = pathlib + "BASE" + "\\win-library" + "\\3.5";
        re.assign("libpath",libPath);
        System.out.println("libpath:" + libPath);
        re.eval(".libPaths(normalizePath(libpath))");
        re.eval("setwd(normalizePath(path))");
        // Load R functions
        
        
        String codeR = mainPath + "\\RCodes" + "\\lmer_uvcov_beta.R";
        re.assign("codeR",codeR);
        re.eval("source(codeR)");
        codeR = mainPath + "\\RCodes" + "\\predict.R";
        re.assign("codeR",codeR);
        re.eval("source(codeR)");
        codeR = mainPath + "\\RCodes" + "\\relfac.R";
        re.assign("codeR",codeR);
        re.eval("source(codeR)");
        codeR = mainPath + "\\RCodes" + "\\theta_optim.R";
        re.assign("codeR",codeR);
        re.eval("source(codeR)"); 
        codeR = mainPath + "\\RCodes" + "\\Models2V4.r";
        re.assign("codeR",codeR);
        re.eval("source(codeR)");
        codeR = mainPath + "\\RCodes" + "\\Read_data.R";
        re.assign("codeR",codeR);
        re.eval("source(codeR)");
        codeR = mainPath + "\\RCodes" + "\\datosSites.R";
        re.assign("codeR",codeR);
        re.eval("source(codeR)");
        codeR = mainPath + "\\RCodes" + "\\Recode.R";
        re.assign("codeR",codeR);
        re.eval("source(codeR)");
        codeR = mainPath + "\\RCodes" + "\\Impute.R";
        re.assign("codeR",codeR);
        re.eval("source(codeR)");
        System.out.print("re"+re.toString());
        
       
        
        
        
        re.eval("datos <- try(ReadData(archivo))");
        re.eval("errorRead <- ifelse(inherits(datos,'try-error'),99,0)");
        REXP outputvalue = re.eval("errorRead");
        if(outputvalue.asDouble() == 99){
            outputvalue = re.eval("datos");
            String messageS = outputvalue.asString();                 
            JOptionPane.showMessageDialog(this, "Unexpected error reading your data set:"+"\n\n"+messageS+"\n\n");
        }
        
        //Visulize all
        BigPanel.setVisible(true);
        ResponsesPanel.setVisible(true);
        Variablespanel.setVisible(true);
        Methodpanel.setVisible(true);
        //REMLVpanel.setVisible(false);
        jScrollPane1.setVisible(true);
        InfoPanel.setVisible(false);
        RandomBox.setSelected(false);
        FixedBox.setSelected(false);
        MarkerBox.setSelected(false);
        RelationBox.setSelected(false);
        BGLRMarker.setSelectedIndex(0);
        PlotFormatBox.setSelectedIndex(0);
        OutputFolder.setText("Analysis1");
        //ExperimentalDesignBox.setSelected(false);
        VarText.setVisible(false);
        //RepText.setVisible(false);
        //BlkText.setVisible(false);
        //RepBox.setVisible(false);
        //BlkBox.setVisible(false);
        GenotypeText.setVisible(true);
        GenotypeBox.setVisible(true);

        jLabel3.setVisible(false);
        BGLRMarker.setVisible(false);
        ResultsPanel.setVisible(false);
        re.eval("gc()");  
    }
    
    public void HacerAnalisis(){
        Analyzing.setVisible(true);
        Recoding.setVisible(false);
        Loading.setVisible(false);
        Imputing.setVisible(false);
        re.assign("path",pathname);
        re.assign("archivo",filename);
        re.eval("setwd(normalizePath(path))");
        re.eval("if(!file.exists('Output_LME4GS')) dir.create('Output_LME4GS')");
        re.eval("path <- paste(path,'Output_LME4GS',sep='/')");
        re.eval("setwd(normalizePath(path))");
        String outfol = OutputFolder.getText();
        re.assign("outputFolder",outfol);
        re.eval("if(file.exists(outputFolder)) creadir <- 'no'");
        re.eval("if(!file.exists(outputFolder)) creadir <- 'si'");
        REXP outputcreadir = re.eval("creadir");
        String messageCreadir = outputcreadir.asString();
        if("si".equals(messageCreadir)){       
            re.eval("dir.create(outputFolder)");
        }else{
            int n = JOptionPane.showConfirmDialog(this,
                "Folder "+outfol+" already exists."+"\n"+
                "Would you like to overwrite outputs in this folder?",
                "Output folder",
                JOptionPane.YES_NO_OPTION);
            if(n==JOptionPane.NO_OPTION){
                return;
            }
        }
        ListModel Responses = ChosenVariables.getModel();
        re.assign("formatPlot",(String) PlotFormatBox.getSelectedItem());
        Double message = null;
        String messageS;
        REXP outputvalue;
        int i;
        re.eval("returnedValue <- 0");
        for(i=0; i<Responses.getSize(); i++){ // for para respuestas a analizar
            re.eval("path <- paste(path,outputFolder,sep='/')");
            String carpeta = re.eval("path").asString();
            re.eval("setwd(normalizePath(path))");
            String Response;
            Response = (String) Responses.getElementAt(i);
            re.assign("yvar",Response);
            re.eval("datos[,yvar]=as.numeric(datos[,yvar])");
            
            re.eval("y=(datos[,yvar]-mean(datos[,yvar],na.rm=TRUE))/sd(datos[,yvar],na.rm=TRUE)");
            
            re.eval("names(y) <- datos[,Gen]");
            re.assign("MarkerBox", "false");
            re.assign("PedigreeBox", "false");
            re.assign("RandomBox", "false");
            re.assign("FixedBox", "false");
            re.assign("ExperimentalDesignBox", "false");
            re.assign("DesignBox", "false");

                if(MarkerBox.isSelected()){ // enviar marker info
                    re.assign("MarkerBox", "true");
                    re.assign("BGLRMarker", (String) BGLRMarker.getSelectedItem());
                }
                if(RelationBox.isSelected()){  // Enviar Pedigree info
                    re.assign("PedigreeBox", "true"); 
                } 

                if(RandomBox.isSelected()){ // Enviar Random info
                    re.assign("RandomBox", "true");
                    ListModel RandomVars = ChosenRandom.getModel();
                    int j;
                    String RandomVar;
                    RandomVar = (String) RandomVars.getElementAt(0);
                    re.assign("TotalRandoms",RandomVar);
                    for(j=1; j<RandomVars.getSize(); j++){
                        RandomVar = (String) RandomVars.getElementAt(j);
                        re.assign("Rvar",RandomVar);
                        re.eval("TotalRandoms <- c(TotalRandoms,Rvar)");
                    }
                }
                if(FixedBox.isSelected()){ // Enviar Random info
                    re.assign("FixedBox", "true");
                    ListModel FixedVars = ChosenFixed.getModel();
                    int j;
                    String FixedVar;
                    FixedVar = (String) FixedVars.getElementAt(0);
                    re.assign("TotalFixeds",FixedVar);
                    for(j=1; j<FixedVars.getSize(); j++){
                        FixedVar = (String) FixedVars.getElementAt(j);
                        re.assign("Rvar",FixedVar);
                        re.eval("TotalFixeds <- c(TotalFixeds,Rvar)");
                    }
                }
                //re.eval("hacerGWAS <- 'false'");
                System.out.println("Evaluating predict_lme4gs");
                re.eval("t <- proc.time()");
                re.eval("ETA <- try(predict_lme4gs(datos, Markers, Pedigree, \n" +
"                                   TotalRandoms, TotalFixeds, \n" +
"                                   Gen, y, yvar, \n" +
"                                   MarkerBox, PedigreeBox, \n" +
"                                   RandomBox, FixedBox))");
                re.eval("tPred <- proc.time() - t");

                
            //}
            
            
            if(wasstop==0){
                re.eval("returnedValue <- 55");
            }else{
                re.eval("if(inherits(ETA,'try-error')){ returnederror <- ETA; returnedValue <- 99}");
            }
            outputvalue = re.eval("returnedValue");
            message = outputvalue.asDouble();
            if(message == 99 || message == 55){
                break;
            }
            System.out.println("Analisis ok");
            if(CVbox.isSelected()){
                re.assign("typeCV", (String) CVtype.getSelectedItem());
                REXP timeCV;
                if(CVtype.getSelectedItem() == "Folds"){
                    re.assign("nfolds", String.valueOf( (int) NumberFolds.getValue()));
                    re.eval("nfolds <- as.numeric(nfolds)");
                    timeCV = re.eval("round(tPred[3]*nfolds/60,2)");
                }else if(CVtype.getSelectedItem() == "Load Sets"){
                    timeCV = re.eval("round(tPred[3]*length(unique(CVSets))/60,2)");
                }else{
                    re.assign("TestSize", String.valueOf( (int) TestingSize.getValue()));
                    re.eval("TestSize <- as.numeric(TestSize)");
                    re.assign("numberCV", String.valueOf( (int) NumberCV.getValue()));
                    re.eval("numberCV <- as.numeric(numberCV)");
                    timeCV = re.eval("round(tPred[3]*numberCV/60,2)");
                }
                int n = JOptionPane.showConfirmDialog(this,
                    "Cross Validation for response '" + Responses.getElementAt(i) + "' would take approximately "+ timeCV.asDouble() +" minutes."+"\n"+
                    "Would you like to continue with Cross Validation?",
                    "Cross Validation Time",
                    JOptionPane.YES_NO_OPTION);
                if(n==JOptionPane.YES_OPTION){  
                    System.out.println("Performing CV: " + (String) CVtype.getSelectedItem());
                    re.eval("CV_ERROR <- try(predict_lme4gs_cv(datos, Markers, Pedigree,\n" +
"                                            TotalRandoms, TotalFixeds,\n" +
"                                            Gen, y, yvar,\n" +
"                                            MarkerBox, PedigreeBox,\n" +
"                                            RandomBox, FixedBox,\n" +
"                                            typeCV, nfolds, CVSets, NumberCV) )");
                    
                    re.eval("if(inherits(CV_ERROR,'try-error')){ returnederror <- CV_ERROR; returnedValue <- 99}");
            
                    outputvalue = re.eval("returnedValue");
                    message = outputvalue.asDouble();
                    if(message == 99 || message == 55){
                        System.out.println("Hacer nanalisis 10" + message);
                        break;
                    }
                    
                    System.out.println("Finishing CV");
                }
            }
        }
        
        REXP outputvaluedir = re.eval("getwd()");
        String outputdir = outputvaluedir.asString();
        actualOutputDir = outputvaluedir.asString();
        

        
        
        if(message == 99){
            outputvalue = re.eval("returnederror");
            messageS = outputvalue.asString();
            outputvalue = re.eval("yvar");
            String ResponseError = outputvalue.asString();
            String Analyzed="";
            for(int j=0; j<i; j++){
                Analyzed = Analyzed +(String) Responses.getElementAt(j) + ", ";
            }
            String NotAnalyzed="";
            for(int j=i+1; j<Responses.getSize(); j++){
                NotAnalyzed = NotAnalyzed +(String) Responses.getElementAt(j) + ", ";
            }
            if(i==0){
                JOptionPane.showMessageDialog(this, "Unexpected error analyzing trait "+ResponseError+":"+"\n\n"+
                    messageS + "\n\n" + "Variables "+ NotAnalyzed + "not analyzed");
            }else if(i==(Responses.getSize()-1)){
                JOptionPane.showMessageDialog(this, "Unexpected error analyzing trait "+ResponseError+":"+"\n\n"+
                    messageS+"\n\n"+"Variables "+ Analyzed + "analyzed");
            }else{
                JOptionPane.showMessageDialog(this, "Unexpected error analyzing trait "+ResponseError+":"+"\n\n"+
                    messageS+"\n\n"+"Variables "+ Analyzed + "analyzed"+
                    "\n"+"Variables "+ NotAnalyzed + "not analyzed");
            }
        }else if(message == 55){
            JOptionPane.showMessageDialog(this, "The Analysis was broken\n"
                + "\nSome results could be saved on:\n"
                + pathname);
        }else if(message == 0){
            JOptionPane.showMessageDialog(this, "Successful Analysis\n"
                + "\nSaved on:\n"
                + pathname);
        }      
        File file;
        file = new File(outputdir);
        DefaultMutableTreeNode node = new DefaultMutableTreeNode(file.getName());
        //node = addNodes(node,file);  
        DefaultTreeModel Dir01;
        Dir01 = new DefaultTreeModel(node);
        File[] subItems = file.listFiles();
        for(File file2 : subItems) {
            if(!file2.isDirectory()){
                node.add(new DefaultMutableTreeNode(file2.getName()));
            }
        } 
        for(File file2 : subItems) {
            if(file2.isDirectory()){
                DefaultMutableTreeNode subroot = new DefaultMutableTreeNode(file2.getName());               
                File[] subItems2 = file2.listFiles();
                if(subItems2.length>0){
                    for(File file3 : subItems2) subroot.add(new DefaultMutableTreeNode(file3.getName()));
                }
                node.add(subroot);  
            }
        }
        TreeResults.setModel(Dir01);
        ResultsPanel.setVisible(true);
        re.eval("gc()");
    }
    
    public void CargarMarkers(){
        try{
        Analyzing.setVisible(false);
        Loading.setVisible(true);
        Recoding.setVisible(false);
        Imputing.setVisible(false);
        
        re.assign("path",pathname);
        re.assign("archivo",filename);
        re.eval("setwd(normalizePath(path))");
        re.eval("if(!file.exists('Output_LME4GS')) dir.create('Output_LME4GS')");
        re.eval("path <- paste(path,'Output_LME4GS',sep='/')");
        re.eval("setwd(normalizePath(path))");
        String outfol = OutputFolder.getText();
        int n = JOptionPane.showConfirmDialog(this,
            "Do you want to keep the folder '"+outfol+"' to save results?"+"\n\n"+
            "If this folder actually exists, some output files could be replaced\n",
            "Output folder",
            JOptionPane.YES_NO_OPTION);
        if(n==JOptionPane.NO_OPTION){
            return;
        }
        
        re.assign("outputFolder",outfol);
        re.eval("if(!file.exists(outputFolder)) dir.create(outputFolder)");
        re.eval("path <- paste(path,outputFolder,sep='/')");
        re.eval("setwd(normalizePath(path))");
              
        //re.assign("path",pathname); 
        re.assign("MarkersDir",filedir);
        re.eval("Markers <- try(read.csv(MarkersDir,header=TRUE,check.names=FALSE,row.names=1,na.strings=c(NA,'.','','?_?','?.?','??'),stringsAsFactors=FALSE))");
        
        //re.eval("output<-try(Models_lme4GS())");
        REXP errorMark = re.eval("inherits(Markers,'try-error')");
        if(errorMark.asInt()==1){
            REXP outputvalue = re.eval("Markers");
            String messageS = outputvalue.asString();
            JOptionPane.showMessageDialog(this, "Unexpected error reading the Markers"+"\n\n"+
                messageS);         
        }else{
            JOptionPane.showMessageDialog(this, "Markers matrix has been loaded succesfully");
            //re.eval("path <- getwd()");
            int k = 0;
            String[] parts;
            parts = filedir.split(":?\\\\");
            int s = parts.length;
            String filename = parts[s-1];
            
            Loading.setVisible(false);
            re.eval("InfoMarkersFoder <- FALSE");
            n = JOptionPane.showConfirmDialog(this,
                "Does the markes need recodification?",
                "Recode Markers",
                JOptionPane.YES_NO_OPTION );
            if(n == JOptionPane.YES_OPTION){
                k = 2;
                Recoding.setVisible(true);
                re.eval("ErrorMark <- 0");
                re.eval("InfoMarkersFoder <- TRUE");
                re.eval("if(!file.exists('Info_Markers')) dir.create('Info_Markers')");
                re.eval("path <- paste(path,'Info_Markers',sep='/')");
                re.eval("setwd(normalizePath(path))");
                re.eval("NO_NA <- which(!is.na(Markers[,1]))");
                re.eval("Char_allele <- try(unlist(strsplit(Markers[NO_NA[1],1],split=character())))");
                re.eval("if(inherits(Char_allele,'try-error')) ErrorMark <- 1");
                errorMark = re.eval("ErrorMark");

                if(errorMark.asInt()==1){
                //} else {
                    REXP outputvalue = re.eval("Char_allele");
                    String messageS = outputvalue.asString();
                    JOptionPane.showMessageDialog(this, "Unexpected error recoding the Markers"+"\n\n"+
                            messageS);
                    return;
                }
                re.eval("separador <- ifelse(length(Char_allele)==3,Char_allele[2],'')");
                re.eval("out <- try(recode(Markers,separador))");
                re.eval("if(inherits(out,'try-error')) ErrorMark <- 1");
                errorMark = re.eval("ErrorMark");
                if(errorMark.asInt()==1){
                    REXP outputvalue = re.eval("out");
                    String messageS = outputvalue.asString();
                    JOptionPane.showMessageDialog(this, "Unexpected error recoding the Markers"+"\n\n"+
                        messageS);
                    return;
                }
                
                re.eval("Markers <- out");
                //re.eval("path <- dirname(getwd())");
                //re.eval("setwd(normalizePath(path))");
                JOptionPane.showMessageDialog(this, "Recodification has been succesful\n\n"+
                        "major allele is coded as 0\n"+
                        "heterosigous is coded as 1\n"+
                        "minor allele is coded as 2\n\n"+
                        "you can see info of allels in file Recode_Markers_Info.csv");
            }
            Recoding.setVisible(true);
            re.eval("Markers <- as.matrix(Markers)");
            Imputing.setVisible(true);
            re.eval("out <- try(Impute(Markers))");
            errorMark = re.eval("inherits(out,'try-error')");
            if(errorMark.asInt()==1){
                REXP outputvalue = re.eval("out");
                String messageS = outputvalue.asString();
                JOptionPane.showMessageDialog(this, "Unexpected error in Markers Quality"+"\n\n"+
                    messageS);
                return;
            }
            re.eval("Markers <- out$X");
            re.eval("messageMono <- out$messageMono");
            re.eval("MonoOMaf <- out$MonoOMaf");
            re.eval("rm(out)");
            re.eval("ncols=ifelse(ncol(Markers)>50, 50, ncol(Markers))");          
            re.eval("nrows=ifelse(nrow(Markers)>50, 50, nrow(Markers))");
            
            REXP rowsMarkers = re.eval("ncols");
            double nrows=rowsMarkers.asDouble();
            REXP colnamesMarkers = re.eval("colnames(Markers)[1:ncols]");
            String [] line = colnamesMarkers.asStringArray();
            modelMarkers = new DefaultTableModel(line, 0);
            re.eval("rowi=1");
            for(int i=0; i<nrows; i++){
                colnamesMarkers = re.eval("as.character(Markers[rowi,1:ncols])");
                line = colnamesMarkers.asStringArray();
                Object[] data = new Object[line.length]; ////problema
                System.arraycopy(line, 0, data, 0, line.length);
                modelMarkers.addRow(data);
                re.eval("rowi=rowi+1");
            }
            REXP messageMono = re.eval("messageMono");
            re.eval("rm(rowi,ncols,nrows)");
            REXP MonoOMAF = re.eval("MonoOMaf");
            if(k==2 || MonoOMAF.asDouble()==1){
                actualOutputDir = re.eval("getwd()").asString();
                JOptionPane.showMessageDialog(this, "Markers Quality process has finished:\n\n"+"Imputation done\n\n"+messageMono.asString()+"\n");
                File file;
                REXP outputvaluedir = re.eval("getwd()");
                String outputdir = outputvaluedir.asString();
                file = new File(outputdir);
                DefaultMutableTreeNode node = new DefaultMutableTreeNode();
                node = addNodes(node,file);  
                DefaultTreeModel Dir01;
                Dir01 = new DefaultTreeModel(node);
                TreeResults.setModel(Dir01);
                
                ResultsPanel.setVisible(true);
                //re.eval("path <- dirname(getwd())");
                //re.eval("setwd(normalizePath(path))");
            }else{
                JOptionPane.showMessageDialog(this, "Markers Quality process has finished:\n\n"+"Imputation done\n\n"+messageMono.asString()+"\n");
            }
            Markername.setText(filename);
        }
        re.eval("gc()");
        }catch (Exception ex){
            ex.printStackTrace();
        }
    }
    
    public void CargarRelationship(){
        Analyzing.setVisible(false);
        Loading.setVisible(true);
        Recoding.setVisible(false);
        Imputing.setVisible(false);
        re.assign("PedDir",filedir);
        re.eval("Pedigree <- try(read.csv(PedDir,header=TRUE,check.names=FALSE,row.names=1))");
        REXP errorMark = re.eval("inherits(Pedigree,'try-error')");
        if(errorMark.asInt()==1){
            REXP outputvalue = re.eval("Pedigree");
            String messageS = outputvalue.asString();
            JOptionPane.showMessageDialog(this, "Unexpected error reading the Pedigree"+"\n\n"+
                messageS);         
        }else{
            JOptionPane.showMessageDialog(this, "Pedigree matrix has been loaded succesfully");
            String[] parts;
            parts = filedir.split(":?\\\\");
            int s = parts.length;
            String filename = parts[s-1];
            Pedigreename.setText(filename);
            re.eval("ncols=ifelse(ncol(Pedigree)>50, 50, ncol(Pedigree))");          
            re.eval("nrows=ifelse(nrow(Pedigree)>50, 50, nrow(Pedigree))");
            REXP rowsMarkers = re.eval("ncols");
            double nrows=rowsMarkers.asDouble();
            REXP colnamesMarkers = re.eval("colnames(Pedigree)[1:ncols]");
            String [] line = colnamesMarkers.asStringArray();
            modelPed = new DefaultTableModel(line, 0);
            re.eval("rowi=1");
            for(int i=0; i<nrows; i++){
                colnamesMarkers = re.eval("as.character(Pedigree[rowi,1:ncols])");
                line = colnamesMarkers.asStringArray();
                Object[] data = new Object[line.length]; ////problema
                System.arraycopy(line, 0, data, 0, line.length);
                modelPed.addRow(data);
                re.eval("rowi=rowi+1");
            }
            re.eval("rm(rowi,ncols,nrows)");
        }
        re.eval("gc()");
    }
    
    public void CargarEnvCovs(){
        Analyzing.setVisible(false);
        Loading.setVisible(true);
        Recoding.setVisible(false);
        Imputing.setVisible(false);
        re.assign("ECDir",filedir);
        re.eval("CovEnv <- try(read.csv(ECDir,header=TRUE,check.names=FALSE))");
        REXP errorEC = re.eval("inherits(CovEnv,'try-error')");
        if(errorEC.asInt()==1){
            REXP outputvalue = re.eval("CovEnv");
            String messageS = outputvalue.asString();
            JOptionPane.showMessageDialog(this, "Unexpected error reading the Environment Covariables"+"\n\n"+
                messageS);         
        }else{
            re.eval("CovEnv <- as.matrix(CovEnv)");
            JOptionPane.showMessageDialog(this, "Environment Covariables matrix has been loaded succesfully");
            String[] parts;
            parts = filedir.split(":?\\\\");
            int s = parts.length;
            String filename = parts[s-1];
            //ECname.setText(filename);
            re.eval("ncols=ifelse(ncol(CovEnv)>50, 50, ncol(CovEnv))");          
            re.eval("nrows=ifelse(nrow(CovEnv)>50, 50, nrow(CovEnv))");
            REXP rowsEC = re.eval("ncols");
            double nrows=rowsEC.asDouble();
            REXP colnamesEC = re.eval("colnames(CovEnv)[1:ncols]");
            String [] line = colnamesEC.asStringArray();
            modelEC = new DefaultTableModel(line, 0);
            re.eval("rowi=1");
            for(int i=0; i<nrows; i++){
                colnamesEC = re.eval("as.character(CovEnv[rowi,1:ncols])");
                line = colnamesEC.asStringArray();
                Object[] data = new Object[line.length]; ////problema
                System.arraycopy(line, 0, data, 0, line.length);
                modelEC.addRow(data);
                re.eval("rowi=rowi+1");
            }
            re.eval("rm(rowi,ncols,nrows)");
        }
        re.eval("gc()");
    }
    
    public void CargarSets(){
        Analyzing.setVisible(false);
        Loading.setVisible(true);
        Recoding.setVisible(false);
        Imputing.setVisible(false);
        re.assign("CVDir",filedir);
        re.eval("CVSets <- try(read.csv(CVDir,header=TRUE,check.names=FALSE))");
        REXP errorEC = re.eval("inherits(CVSets,'try-error')");
        if(errorEC.asInt()==1){
            REXP outputvalue = re.eval("CVSets");
            String messageS = outputvalue.asString();
            JOptionPane.showMessageDialog(this, "Unexpected error reading the Cross Validation Sets"+"\n\n"+
                messageS);         
        }else{
            re.eval("CVSets <- as.matrix(CVSets)");
            JOptionPane.showMessageDialog(this, "Cross Validation Set has been loaded succesfully");
            String[] parts;
            parts = filedir.split(":?\\\\");
            int s = parts.length;
            String filename = parts[s-1];
            CVSetname.setText(filename);
            re.eval("ncols=ifelse(ncol(CVSets)>50, 50, ncol(CVSets))");          
            re.eval("nrows=ifelse(nrow(CVSets)>50, 50, nrow(CVSets))");
            REXP rowsCV = re.eval("nrows");
            double nrows=rowsCV.asDouble();
            REXP colnamesEC = re.eval("colnames(CVSets)[1:ncols]");
            String [] line = colnamesEC.asStringArray();
            modelCV = new DefaultTableModel(line, 0);
            re.eval("rowi=1");
            for(int i=0; i<nrows; i++){
                colnamesEC = re.eval("as.character(CVSets[rowi,1:ncols])");
                line = colnamesEC.asStringArray();
                Object[] data = new Object[line.length]; ////problema
                System.arraycopy(line, 0, data, 0, line.length);
                modelCV.addRow(data);
                re.eval("rowi=rowi+1");
            }
            re.eval("rm(rowi,ncols,nrows)");
        }
        re.eval("gc()");
    }
    
    public void task() throws IOException{
        /////////////////////// desabilitar botones /////////////////////////
        // main menu //
        Open.setEnabled(false);
        Analyze.setEnabled(false);
        Help.setEnabled(false);
        // menu model //
        BGLRMarker.setEnabled(false);
        RelationBox.setEnabled(false);
        RandomBox.setEnabled(false);
        FixedBox.setEnabled(false);
        MarkerBox.setEnabled(false);
        LoadMarker.setEnabled(false);
        SeeMarker.setEnabled(false);       
        LoadPedigree.setEnabled(false);
        SeePedigree.setEnabled(false);
        PlotFormatBox.setEnabled(false);
        OutputFolder.setEnabled(false);
         // menu design //
        //ExperimentalDesignBox.setEnabled(false);
        //DesignBox.setEnabled(false);
        //RepBox.setEnabled(false);
        //BlkBox.setEnabled(false);
        GenotypeBox.setEnabled(false);

        // CV Module //
        CVbox.setEnabled(false);
        CVbutton.setEnabled(false);
        // responses // 
        SelectResponses.setEnabled(false);
        DropResponses.setEnabled(false);
        ListVariables.setEnabled(false);
        ChosenVariables.setEnabled(false);

        SelectRandom.setEnabled(false);
        DropRandom.setEnabled(false);
        ListRandom.setEnabled(false);
        ChosenRandom.setEnabled(false);  
        // Fixed //
        SelectFixed.setEnabled(false);
        DropFixed.setEnabled(false);
        ListFixed.setEnabled(false);
        ChosenFixed.setEnabled(false);   
        ////////////////////// Option to run ////////////////////////////////////
        if(worktodo==1){
            CargarDatos();
        }else if(worktodo==2){
            CargarMarkers();
        }else if(worktodo==3){
            CargarRelationship();
        }else if(worktodo==4){
            CargarEnvCovs();
        }else if(worktodo==5){
            HacerAnalisis();
        }else if(worktodo==6){
            CargarSets();
        }
        /////////////////////// habilitar botones /////////////////////////
        // main menu //
        Open.setEnabled(true);
        Analyze.setEnabled(true);
        Help.setEnabled(true);
        // menu model //
        

        MarkerBox.setEnabled(true);
        
        if(MarkerBox.isSelected()){
            BGLRMarker.setEnabled(true);
            //BGLRMarker.setVisible(true);
            LoadMarker.setVisible(true);
            SeeMarker.setVisible(true);
            Markername.setVisible(true);
        }else{
            //BGLRMarker.setVisible(false);
            LoadMarker.setVisible(false);
            SeeMarker.setVisible(false);
            Markername.setVisible(false);
        }
        RelationBox.setEnabled(true);
        if(RelationBox.isSelected()){
            LoadPedigree.setVisible(true);
            Pedigreename.setVisible(true);
            SeePedigree.setVisible(true);
        }else{
            LoadPedigree.setVisible(false);
            Pedigreename.setVisible(false);
            SeePedigree.setVisible(false);
        }
        RandomBox.setEnabled(true);
        if(RandomBox.isSelected()){
            Ran_Eff.setVisible(false);
        }else{
            Ran_Eff.setVisible(false);
        }
        FixedBox.setEnabled(true);
        if(FixedBox.isSelected()){
            Fix_Eff.setVisible(true);
        }else{
            Fix_Eff.setVisible(false);
        }
        LoadMarker.setEnabled(true);
        SeeMarker.setEnabled(true);       
        LoadPedigree.setEnabled(true);
        SeePedigree.setEnabled(true);
        PlotFormatBox.setEnabled(true);
        OutputFolder.setEnabled(true);
        // menu design //
        
        /*ExperimentalDesignBox.setEnabled(true);
        if(ExperimentalDesignBox.isSelected()){
            DesignBox.setEnabled(true);
        }else{
            DesignBox.setEnabled(false);
        }*/
        //SiteBox.setEnabled(true);
        /*RepBox.setEnabled(true);
        if(DesignBox.getSelectedItem()=="RCBD"){
            BlkBox.setEnabled(false);
        }else{
            BlkBox.setEnabled(true);
        }*/
        //GenotypeBox.setEnabled(true);
        
        GenotypeBox.setEnabled(true);


            //GenotypeText.setVisible(false);
            //GenotypeBox.setVisible(false);
        
        // CV Module //        
        CVbox.setEnabled(true);
        CVbutton.setEnabled(true);
        // responses //
        SelectResponses.setEnabled(true);
        DropResponses.setEnabled(true);
        ListVariables.setEnabled(true);
        ChosenVariables.setEnabled(true);
        // sites // 
        //SelectSites.setEnabled(true);
        //DropSites.setEnabled(true);
        //ListSites.setEnabled(true);
        //ChosenSites.setEnabled(true);
        // Random //
        SelectRandom.setEnabled(true);
        DropRandom.setEnabled(true);
        ListRandom.setEnabled(true);
        ChosenRandom.setEnabled(true);
        // Fixed //
        SelectFixed.setEnabled(true);
        DropFixed.setEnabled(true);
        ListFixed.setEnabled(true);
        ChosenFixed.setEnabled(true); 
    }
    
    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        db = new javax.swing.JFileChooser();
        SitesTo = new javax.swing.JPanel();
        jLabel16 = new javax.swing.JLabel();
        jLabel17 = new javax.swing.JLabel();
        jLabel18 = new javax.swing.JLabel();
        ScrollVariables1 = new javax.swing.JScrollPane();
        ListSites = new javax.swing.JList();
        jScrollPane4 = new javax.swing.JScrollPane();
        ChosenSites = new javax.swing.JList();
        SelectSites = new javax.swing.JButton();
        DropSites = new javax.swing.JButton();
        ExperimentalDesignBox = new javax.swing.JCheckBox();
        DesignBox = new javax.swing.JComboBox();
        RepBox = new javax.swing.JComboBox();
        RepText = new javax.swing.JLabel();
        BlkBox = new javax.swing.JComboBox();
        BlkText = new javax.swing.JLabel();
        VarText = new javax.swing.JLabel();
        jSeparator4 = new javax.swing.JSeparator();
        CVModule = new javax.swing.JDialog(this);
        BigPanelCV = new javax.swing.JPanel();
        CVpanel = new javax.swing.JPanel();
        OkCV = new javax.swing.JButton();
        CVtype = new javax.swing.JComboBox<>();
        jLabel6 = new javax.swing.JLabel();
        jPanel3 = new javax.swing.JPanel();
        NumberFolds = new javax.swing.JSpinner();
        NumberFoldsLabel = new javax.swing.JLabel();
        TestingSize = new javax.swing.JSpinner();
        NumberCV = new javax.swing.JSpinner();
        TestingSizeLabel = new javax.swing.JLabel();
        NumberCVLabel = new javax.swing.JLabel();
        LoadCVSets = new javax.swing.JButton();
        CVSetname = new javax.swing.JTextField();
        SeeCVSets = new javax.swing.JButton();
        jLabel31 = new javax.swing.JLabel();
        jScrollPane7 = new javax.swing.JScrollPane();
        jPanel2 = new javax.swing.JPanel();
        jPanel1 = new javax.swing.JPanel();
        Open = new javax.swing.JButton();
        Analyze = new javax.swing.JButton();
        Help = new javax.swing.JButton();
        LogoBSU = new javax.swing.JLabel();
        LogoCIMMYT = new javax.swing.JLabel();
        BigPanel = new javax.swing.JPanel();
        Methodpanel = new javax.swing.JPanel();
        jLabel1 = new javax.swing.JLabel();
        MarkerBox = new javax.swing.JCheckBox();
        RelationBox = new javax.swing.JCheckBox();
        RandomBox = new javax.swing.JCheckBox();
        FixedBox = new javax.swing.JCheckBox();
        jSeparator6 = new javax.swing.JSeparator();
        jLabel3 = new javax.swing.JLabel();
        BGLRMarker = new javax.swing.JComboBox();
        LoadMarker = new javax.swing.JButton();
        SeeMarker = new javax.swing.JButton();
        Markername = new javax.swing.JTextField();
        LoadPedigree = new javax.swing.JButton();
        SeePedigree = new javax.swing.JButton();
        Pedigreename = new javax.swing.JTextField();
        jSeparator3 = new javax.swing.JSeparator();
        jLabel2 = new javax.swing.JLabel();
        PlotFormatBox = new javax.swing.JComboBox();
        jLabel23 = new javax.swing.JLabel();
        OutputFolder = new javax.swing.JTextField();
        Variablespanel = new javax.swing.JPanel();
        InfoPanelVar = new javax.swing.JLabel();
        GenotypeText = new javax.swing.JLabel();
        GenotypeBox = new javax.swing.JComboBox();
        jSeparator1 = new javax.swing.JSeparator();
        jSeparator2 = new javax.swing.JSeparator();
        CVbutton = new javax.swing.JButton();
        CVbox = new javax.swing.JCheckBox();
        SVBox = new javax.swing.JCheckBox();
        ResponsesPanel = new javax.swing.JPanel();
        jLabel13 = new javax.swing.JLabel();
        jLabel14 = new javax.swing.JLabel();
        ScrollVariables = new javax.swing.JScrollPane();
        ListVariables = new javax.swing.JList();
        SelectResponses = new javax.swing.JButton();
        jLabel15 = new javax.swing.JLabel();
        jScrollPane3 = new javax.swing.JScrollPane();
        ChosenVariables = new javax.swing.JList();
        DropResponses = new javax.swing.JButton();
        Ran_Eff = new javax.swing.JPanel();
        jLabel11 = new javax.swing.JLabel();
        jLabel22 = new javax.swing.JLabel();
        ScrollVariables2 = new javax.swing.JScrollPane();
        ListRandom = new javax.swing.JList();
        SelectRandom = new javax.swing.JButton();
        jLabel21 = new javax.swing.JLabel();
        jScrollPane5 = new javax.swing.JScrollPane();
        ChosenRandom = new javax.swing.JList();
        DropRandom = new javax.swing.JButton();
        Fix_Eff = new javax.swing.JPanel();
        jLabel12 = new javax.swing.JLabel();
        jLabel29 = new javax.swing.JLabel();
        ScrollVariables3 = new javax.swing.JScrollPane();
        ListFixed = new javax.swing.JList();
        SelectFixed = new javax.swing.JButton();
        jLabel28 = new javax.swing.JLabel();
        jScrollPane6 = new javax.swing.JScrollPane();
        ChosenFixed = new javax.swing.JList();
        DropFixed = new javax.swing.JButton();
        InfoPanel = new javax.swing.JScrollPane();
        Info = new javax.swing.JTextArea();
        jScrollPane1 = new javax.swing.JScrollPane();
        PanelMenus = new javax.swing.JTable();
        ResultsPanel = new javax.swing.JPanel();
        jLabel19 = new javax.swing.JLabel();
        jScrollPane2 = new javax.swing.JScrollPane();
        TreeResults = new javax.swing.JTree();
        PanelBar = new javax.swing.JPanel();
        Imputing = new javax.swing.JLabel();
        Recoding = new javax.swing.JLabel();
        Loading = new javax.swing.JLabel();
        Analyzing = new javax.swing.JLabel();
        Pbarra = new javax.swing.JProgressBar();
        StopProcess = new java.awt.Button();

        SitesTo.setBorder(javax.swing.BorderFactory.createMatteBorder(1, 1, 1, 1, new java.awt.Color(153, 153, 0)));
        SitesTo.setPreferredSize(new java.awt.Dimension(200, 400));

        jLabel16.setFont(new java.awt.Font("Tahoma", 1, 12)); // NOI18N
        jLabel16.setText("Sites to Analyze");

        jLabel17.setFont(new java.awt.Font("Tahoma", 1, 11)); // NOI18N
        jLabel17.setText("Select one or more");

        jLabel18.setFont(new java.awt.Font("Tahoma", 1, 11)); // NOI18N
        jLabel18.setText("Selected");

        ScrollVariables1.setAutoscrolls(true);
        ScrollVariables1.setPreferredSize(new java.awt.Dimension(178, 115));

        ListSites.setFont(new java.awt.Font("Tahoma", 1, 11)); // NOI18N
        ListSites.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                ListSitesMouseClicked(evt);
            }
        });
        ScrollVariables1.setViewportView(ListSites);

        jScrollPane4.setAutoscrolls(true);
        jScrollPane4.setPreferredSize(new java.awt.Dimension(178, 115));

        ChosenSites.setFont(new java.awt.Font("Tahoma", 1, 11)); // NOI18N
        ChosenSites.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                ChosenSitesMouseClicked(evt);
            }
        });
        jScrollPane4.setViewportView(ChosenSites);

        SelectSites.setText("Select All");
        SelectSites.setPreferredSize(new java.awt.Dimension(178, 25));
        SelectSites.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                SelectSitesActionPerformed(evt);
            }
        });

        DropSites.setText("Drop All");
        DropSites.setPreferredSize(new java.awt.Dimension(178, 25));
        DropSites.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                DropSitesActionPerformed(evt);
            }
        });

        ExperimentalDesignBox.setFont(new java.awt.Font("Tahoma", 1, 12)); // NOI18N
        ExperimentalDesignBox.setText("Experimental Design");
        ExperimentalDesignBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                ExperimentalDesignBoxActionPerformed(evt);
            }
        });

        DesignBox.setFont(new java.awt.Font("Tahoma", 1, 11)); // NOI18N
        DesignBox.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Lattice", "RCBD" }));
        DesignBox.setMinimumSize(new java.awt.Dimension(64, 0));
        DesignBox.setPreferredSize(new java.awt.Dimension(199, 25));
        DesignBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                DesignBoxActionPerformed(evt);
            }
        });

        RepBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                RepBoxActionPerformed(evt);
            }
        });

        RepText.setFont(new java.awt.Font("Tahoma", 1, 11)); // NOI18N
        RepText.setText("Replicate:");
        RepText.setPreferredSize(new java.awt.Dimension(65, 14));

        BlkBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                BlkBoxActionPerformed(evt);
            }
        });

        BlkText.setFont(new java.awt.Font("Tahoma", 1, 11)); // NOI18N
        BlkText.setText("Block:");
        BlkText.setPreferredSize(new java.awt.Dimension(65, 14));

        VarText.setFont(new java.awt.Font("Tahoma", 1, 12)); // NOI18N
        VarText.setText("Variables Selection");

        javax.swing.GroupLayout SitesToLayout = new javax.swing.GroupLayout(SitesTo);
        SitesTo.setLayout(SitesToLayout);
        SitesToLayout.setHorizontalGroup(
            SitesToLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(SitesToLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(SitesToLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jSeparator4)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, SitesToLayout.createSequentialGroup()
                        .addGap(10, 10, 10)
                        .addGroup(SitesToLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addGroup(SitesToLayout.createSequentialGroup()
                                .addComponent(RepText, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                .addGap(32, 32, 32))
                            .addGroup(SitesToLayout.createSequentialGroup()
                                .addComponent(BlkText, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)))
                        .addGroup(SitesToLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                            .addComponent(BlkBox, javax.swing.GroupLayout.Alignment.TRAILING, 0, 71, Short.MAX_VALUE)
                            .addComponent(RepBox, javax.swing.GroupLayout.Alignment.TRAILING, 0, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)))
                    .addGroup(SitesToLayout.createSequentialGroup()
                        .addGroup(SitesToLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jLabel16, javax.swing.GroupLayout.PREFERRED_SIZE, 138, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addComponent(jLabel17)
                            .addComponent(ScrollVariables1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addComponent(jLabel18)
                            .addGroup(SitesToLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING, false)
                                .addComponent(jScrollPane4, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addComponent(DropSites, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                .addComponent(SelectSites, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                            .addGroup(SitesToLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING, false)
                                .addComponent(DesignBox, javax.swing.GroupLayout.Alignment.LEADING, 0, 0, Short.MAX_VALUE)
                                .addComponent(ExperimentalDesignBox, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                            .addComponent(VarText))
                        .addGap(0, 0, Short.MAX_VALUE)))
                .addContainerGap())
        );
        SitesToLayout.setVerticalGroup(
            SitesToLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(SitesToLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jLabel16)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jLabel17)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(ScrollVariables1, javax.swing.GroupLayout.PREFERRED_SIZE, 34, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(SelectSites, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jLabel18)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jScrollPane4, javax.swing.GroupLayout.PREFERRED_SIZE, 27, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(18, 18, 18)
                .addComponent(DropSites, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(ExperimentalDesignBox)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(DesignBox, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addComponent(jSeparator4, javax.swing.GroupLayout.PREFERRED_SIZE, 10, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(VarText)
                .addGap(18, 18, 18)
                .addGroup(SitesToLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(RepBox, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(RepText, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(SitesToLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(BlkBox, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(BlkText, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap())
        );

        CVModule.setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        CVModule.setTitle("Cross Validation Module");
        CVModule.setModalExclusionType(null);
        CVModule.setModalityType(java.awt.Dialog.ModalityType.DOCUMENT_MODAL);
        CVModule.setResizable(false);

        BigPanelCV.setBackground(new java.awt.Color(255, 255, 255));
        BigPanelCV.setBorder(javax.swing.BorderFactory.createBevelBorder(javax.swing.border.BevelBorder.RAISED));
        BigPanelCV.setPreferredSize(new java.awt.Dimension(416, 378));

        CVpanel.setBorder(javax.swing.BorderFactory.createMatteBorder(1, 1, 1, 1, new java.awt.Color(204, 204, 255)));
        CVpanel.setPreferredSize(new java.awt.Dimension(394, 261));

        OkCV.setText("Ok");
        OkCV.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                OkCVActionPerformed(evt);
            }
        });

        CVtype.setModel(new javax.swing.DefaultComboBoxModel<>(new String[] { "Folds", "Training\\Testing %", "Load Sets" }));
        CVtype.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                CVtypeActionPerformed(evt);
            }
        });

        jLabel6.setText("Cross Validation Type:");

        jPanel3.setBorder(new javax.swing.border.MatteBorder(null));

        NumberFolds.setModel(new javax.swing.SpinnerNumberModel(10, 2, null, 1));
        NumberFolds.setPreferredSize(new java.awt.Dimension(50, 20));

        NumberFoldsLabel.setText("Number of Folds");

        TestingSize.setModel(new javax.swing.SpinnerNumberModel(1, 1, 99, 1));
        TestingSize.setPreferredSize(new java.awt.Dimension(50, 20));

        NumberCV.setModel(new javax.swing.SpinnerNumberModel(1, 1, null, 1));
        NumberCV.setPreferredSize(new java.awt.Dimension(50, 20));

        TestingSizeLabel.setText("Testing set size (percentage of missing data) ");

        NumberCVLabel.setText("Number of cross validations");

        LoadCVSets.setText("Load CV Sets");
        LoadCVSets.setPreferredSize(new java.awt.Dimension(115, 25));
        LoadCVSets.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                LoadCVSetsActionPerformed(evt);
            }
        });

        CVSetname.setEditable(false);
        CVSetname.setToolTipText("");
        CVSetname.setPreferredSize(new java.awt.Dimension(258, 25));
        CVSetname.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                CVSetnameActionPerformed(evt);
            }
        });

        SeeCVSets.setText("See CV Sets");
        SeeCVSets.setPreferredSize(new java.awt.Dimension(115, 25));
        SeeCVSets.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                SeeCVSetsActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout jPanel3Layout = new javax.swing.GroupLayout(jPanel3);
        jPanel3.setLayout(jPanel3Layout);
        jPanel3Layout.setHorizontalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel3Layout.createSequentialGroup()
                        .addComponent(NumberFoldsLabel)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(NumberFolds, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel3Layout.createSequentialGroup()
                        .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(TestingSizeLabel)
                            .addComponent(NumberCVLabel))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                            .addComponent(TestingSize, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                            .addComponent(NumberCV, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)))
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel3Layout.createSequentialGroup()
                        .addComponent(LoadCVSets, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addGap(28, 28, 28)
                        .addComponent(SeeCVSets, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                    .addComponent(CVSetname, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addContainerGap())
        );
        jPanel3Layout.setVerticalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(NumberFolds, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(NumberFoldsLabel))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(TestingSize, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(TestingSizeLabel))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(NumberCV, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(NumberCVLabel))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(LoadCVSets, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(SeeCVSets, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(CVSetname, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(23, Short.MAX_VALUE))
        );

        javax.swing.GroupLayout CVpanelLayout = new javax.swing.GroupLayout(CVpanel);
        CVpanel.setLayout(CVpanelLayout);
        CVpanelLayout.setHorizontalGroup(
            CVpanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(CVpanelLayout.createSequentialGroup()
                .addGroup(CVpanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, CVpanelLayout.createSequentialGroup()
                        .addGap(0, 0, Short.MAX_VALUE)
                        .addComponent(OkCV, javax.swing.GroupLayout.PREFERRED_SIZE, 74, javax.swing.GroupLayout.PREFERRED_SIZE))
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, CVpanelLayout.createSequentialGroup()
                        .addContainerGap()
                        .addGroup(CVpanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jPanel3, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, CVpanelLayout.createSequentialGroup()
                                .addComponent(jLabel6)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                .addComponent(CVtype, javax.swing.GroupLayout.PREFERRED_SIZE, 162, javax.swing.GroupLayout.PREFERRED_SIZE)))))
                .addContainerGap())
        );
        CVpanelLayout.setVerticalGroup(
            CVpanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(CVpanelLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(CVpanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(CVtype, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel6))
                .addGap(18, 18, 18)
                .addComponent(jPanel3, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addComponent(OkCV)
                .addContainerGap())
        );

        jLabel31.setFont(new java.awt.Font("Tahoma", 1, 14)); // NOI18N
        jLabel31.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
        jLabel31.setText("Setting Cross Validation");
        jLabel31.setPreferredSize(new java.awt.Dimension(396, 17));

        javax.swing.GroupLayout BigPanelCVLayout = new javax.swing.GroupLayout(BigPanelCV);
        BigPanelCV.setLayout(BigPanelCVLayout);
        BigPanelCVLayout.setHorizontalGroup(
            BigPanelCVLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(BigPanelCVLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(BigPanelCVLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jLabel31, javax.swing.GroupLayout.DEFAULT_SIZE, 392, Short.MAX_VALUE)
                    .addComponent(CVpanel, javax.swing.GroupLayout.DEFAULT_SIZE, 392, Short.MAX_VALUE))
                .addContainerGap())
        );
        BigPanelCVLayout.setVerticalGroup(
            BigPanelCVLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, BigPanelCVLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jLabel31, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(18, 18, 18)
                .addComponent(CVpanel, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        javax.swing.GroupLayout CVModuleLayout = new javax.swing.GroupLayout(CVModule.getContentPane());
        CVModule.getContentPane().setLayout(CVModuleLayout);
        CVModuleLayout.setHorizontalGroup(
            CVModuleLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(BigPanelCV, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
        );
        CVModuleLayout.setVerticalGroup(
            CVModuleLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(CVModuleLayout.createSequentialGroup()
                .addComponent(BigPanelCV, javax.swing.GroupLayout.PREFERRED_SIZE, 318, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(0, 0, Short.MAX_VALUE))
        );

        setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE);
        setIconImage(Toolkit.getDefaultToolkit().getImage("Images/logolme4gs.png"));
        setMinimumSize(new java.awt.Dimension(800, 600));

        jScrollPane7.setPreferredSize(new java.awt.Dimension(1198, 600));

        jPanel2.setBackground(new java.awt.Color(204, 204, 255));
        jPanel2.setCursor(new java.awt.Cursor(java.awt.Cursor.DEFAULT_CURSOR));
        jPanel2.setDoubleBuffered(false);
        jPanel2.setPreferredSize(new java.awt.Dimension(1196, 633));

        jPanel1.setBackground(new java.awt.Color(255, 255, 255));
        jPanel1.setBorder(javax.swing.BorderFactory.createBevelBorder(javax.swing.border.BevelBorder.RAISED));
        jPanel1.setPreferredSize(new java.awt.Dimension(1196, 89));

        Open.setText("Open phenotypic data");
        Open.setPreferredSize(new java.awt.Dimension(100, 30));
        Open.setRolloverEnabled(false);
        Open.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                OpenActionPerformed(evt);
            }
        });

        Analyze.setText("Analyze");
        Analyze.setPreferredSize(new java.awt.Dimension(100, 30));
        Analyze.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                AnalyzeActionPerformed(evt);
            }
        });

        Help.setText("Help");
        Help.setPreferredSize(new java.awt.Dimension(100, 30));
        Help.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                HelpActionPerformed(evt);
            }
        });

        LogoBSU.setIcon(new javax.swing.ImageIcon(getClass().getResource("/CIMMYT Official Logo Green small.jpg"))); // NOI18N

        LogoCIMMYT.setIcon(new javax.swing.ImageIcon(getClass().getResource("/BSUlogo_purple_small.jpg"))); // NOI18N

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(Open, javax.swing.GroupLayout.PREFERRED_SIZE, 160, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(Analyze, javax.swing.GroupLayout.PREFERRED_SIZE, 160, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(Help, javax.swing.GroupLayout.PREFERRED_SIZE, 160, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addComponent(LogoCIMMYT)
                .addGap(18, 18, 18)
                .addComponent(LogoBSU)
                .addContainerGap())
        );
        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(LogoBSU, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, 64, Short.MAX_VALUE)
                    .addComponent(LogoCIMMYT, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addGap(8, 8, 8))
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addGap(27, 27, 27)
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(Help, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(Analyze, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(Open, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        BigPanel.setBackground(new java.awt.Color(204, 204, 255));

        Methodpanel.setBorder(javax.swing.BorderFactory.createBevelBorder(javax.swing.border.BevelBorder.RAISED));
        Methodpanel.setPreferredSize(new java.awt.Dimension(280, 406));

        jLabel1.setFont(new java.awt.Font("Tahoma", 1, 12)); // NOI18N
        jLabel1.setText("Prediction Analysis:");

        MarkerBox.setText("Markers");
        MarkerBox.setPreferredSize(new java.awt.Dimension(80, 20));
        MarkerBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                MarkerBoxActionPerformed(evt);
            }
        });

        RelationBox.setText("Relationship matrix");
        RelationBox.setPreferredSize(new java.awt.Dimension(136, 20));
        RelationBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                RelationBoxActionPerformed(evt);
            }
        });

        RandomBox.setText("Random effects");
        RandomBox.setPreferredSize(new java.awt.Dimension(115, 20));
        RandomBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                RandomBoxActionPerformed(evt);
            }
        });

        FixedBox.setText("Fixed effects");
        FixedBox.setPreferredSize(new java.awt.Dimension(135, 20));
        FixedBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                FixedBoxActionPerformed(evt);
            }
        });

        jLabel3.setText("Model for Markers:");

        BGLRMarker.setFont(new java.awt.Font("Tahoma", 1, 11)); // NOI18N
        BGLRMarker.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Ridge", "GBLUP", "LASSO", "BayesA", "BayesB" }));
        BGLRMarker.setPreferredSize(new java.awt.Dimension(100, 23));
        BGLRMarker.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                BGLRMarkerActionPerformed(evt);
            }
        });

        LoadMarker.setText("Load Markers");
        LoadMarker.setPreferredSize(new java.awt.Dimension(115, 25));
        LoadMarker.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                LoadMarkerActionPerformed(evt);
            }
        });

        SeeMarker.setText("See Markers");
        SeeMarker.setPreferredSize(new java.awt.Dimension(115, 25));
        SeeMarker.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                SeeMarkerActionPerformed(evt);
            }
        });

        Markername.setEditable(false);
        Markername.setPreferredSize(new java.awt.Dimension(258, 25));
        Markername.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                MarkernameActionPerformed(evt);
            }
        });

        LoadPedigree.setText("Load Relation");
        LoadPedigree.setPreferredSize(new java.awt.Dimension(115, 25));
        LoadPedigree.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                LoadPedigreeActionPerformed(evt);
            }
        });

        SeePedigree.setText("See Relation");
        SeePedigree.setPreferredSize(new java.awt.Dimension(115, 25));
        SeePedigree.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                SeePedigreeActionPerformed(evt);
            }
        });

        Pedigreename.setEditable(false);
        Pedigreename.setToolTipText("");
        Pedigreename.setPreferredSize(new java.awt.Dimension(258, 25));
        Pedigreename.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                PedigreenameActionPerformed(evt);
            }
        });

        jLabel2.setFont(new java.awt.Font("Tahoma", 1, 12)); // NOI18N
        jLabel2.setText("Plots Format:");

        PlotFormatBox.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "PDF (*.pdf)", "PNG (*.png)", "Metafile (*.wmf)" }));
        PlotFormatBox.setPreferredSize(new java.awt.Dimension(150, 20));

        jLabel23.setFont(new java.awt.Font("Tahoma", 1, 12)); // NOI18N
        jLabel23.setText("Output folder:");

        OutputFolder.setText("Analysis1");
        OutputFolder.setMinimumSize(new java.awt.Dimension(130, 20));
        OutputFolder.setPreferredSize(new java.awt.Dimension(150, 25));
        OutputFolder.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                OutputFolderActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout MethodpanelLayout = new javax.swing.GroupLayout(Methodpanel);
        Methodpanel.setLayout(MethodpanelLayout);
        MethodpanelLayout.setHorizontalGroup(
            MethodpanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jSeparator3)
            .addComponent(jSeparator6)
            .addGroup(MethodpanelLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(MethodpanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(MethodpanelLayout.createSequentialGroup()
                        .addGroup(MethodpanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jLabel23)
                            .addComponent(jLabel2))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                        .addGroup(MethodpanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(OutputFolder, javax.swing.GroupLayout.DEFAULT_SIZE, 151, Short.MAX_VALUE)
                            .addComponent(PlotFormatBox, 0, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)))
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, MethodpanelLayout.createSequentialGroup()
                        .addComponent(LoadPedigree, javax.swing.GroupLayout.DEFAULT_SIZE, 114, Short.MAX_VALUE)
                        .addGap(28, 28, 28)
                        .addComponent(SeePedigree, javax.swing.GroupLayout.DEFAULT_SIZE, 114, Short.MAX_VALUE))
                    .addComponent(Pedigreename, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, 256, Short.MAX_VALUE)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, MethodpanelLayout.createSequentialGroup()
                        .addComponent(LoadMarker, javax.swing.GroupLayout.DEFAULT_SIZE, 114, Short.MAX_VALUE)
                        .addGap(28, 28, 28)
                        .addComponent(SeeMarker, javax.swing.GroupLayout.DEFAULT_SIZE, 114, Short.MAX_VALUE))
                    .addComponent(Markername, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, 256, Short.MAX_VALUE)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, MethodpanelLayout.createSequentialGroup()
                        .addGroup(MethodpanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(MarkerBox, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addComponent(RandomBox, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addGroup(MethodpanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(FixedBox, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addComponent(RelationBox, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)))
                    .addGroup(MethodpanelLayout.createSequentialGroup()
                        .addComponent(jLabel3)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(BGLRMarker, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                    .addGroup(MethodpanelLayout.createSequentialGroup()
                        .addComponent(jLabel1)
                        .addGap(0, 0, Short.MAX_VALUE)))
                .addContainerGap())
        );
        MethodpanelLayout.setVerticalGroup(
            MethodpanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, MethodpanelLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jLabel1)
                .addGap(32, 32, 32)
                .addGroup(MethodpanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(RelationBox, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(MarkerBox, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(MethodpanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(FixedBox, javax.swing.GroupLayout.PREFERRED_SIZE, 20, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(RandomBox, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jSeparator6, javax.swing.GroupLayout.PREFERRED_SIZE, 10, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(MethodpanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel3)
                    .addComponent(BGLRMarker, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(MethodpanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(LoadMarker, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(SeeMarker, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(Markername, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(MethodpanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(LoadPedigree, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(SeePedigree, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(Pedigreename, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 14, Short.MAX_VALUE)
                .addComponent(jSeparator3, javax.swing.GroupLayout.PREFERRED_SIZE, 10, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(MethodpanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel2)
                    .addComponent(PlotFormatBox, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(MethodpanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel23)
                    .addComponent(OutputFolder, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap())
        );

        Variablespanel.setBorder(javax.swing.BorderFactory.createBevelBorder(javax.swing.border.BevelBorder.RAISED));
        Variablespanel.setMaximumSize(new java.awt.Dimension(300, 400));
        Variablespanel.setPreferredSize(new java.awt.Dimension(270, 406));

        InfoPanelVar.setFont(new java.awt.Font("Tahoma", 1, 12)); // NOI18N
        InfoPanelVar.setText("Genotype selection:");

        GenotypeText.setFont(new java.awt.Font("Tahoma", 1, 11)); // NOI18N
        GenotypeText.setText("Genotype (L):");
        GenotypeText.setPreferredSize(new java.awt.Dimension(81, 14));

        GenotypeBox.setMaximumSize(new java.awt.Dimension(200, 32767));
        GenotypeBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                GenotypeBoxActionPerformed(evt);
            }
        });

        CVbutton.setText("CV Module");
        CVbutton.setPreferredSize(new java.awt.Dimension(100, 25));
        CVbutton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                CVbuttonActionPerformed(evt);
            }
        });

        CVbox.setFont(new java.awt.Font("Tahoma", 0, 12)); // NOI18N
        CVbox.setText("Cross Validation");
        CVbox.setPreferredSize(new java.awt.Dimension(120, 30));
        CVbox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                CVboxActionPerformed(evt);
            }
        });

        SVBox.setText(" Standardize Response Variables");
        SVBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                SVBoxActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout VariablespanelLayout = new javax.swing.GroupLayout(Variablespanel);
        Variablespanel.setLayout(VariablespanelLayout);
        VariablespanelLayout.setHorizontalGroup(
            VariablespanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jSeparator1)
            .addComponent(jSeparator2)
            .addGroup(VariablespanelLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(VariablespanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(VariablespanelLayout.createSequentialGroup()
                        .addComponent(GenotypeText, javax.swing.GroupLayout.PREFERRED_SIZE, 95, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(GenotypeBox, javax.swing.GroupLayout.PREFERRED_SIZE, 139, javax.swing.GroupLayout.PREFERRED_SIZE))
                    .addGroup(VariablespanelLayout.createSequentialGroup()
                        .addGroup(VariablespanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(InfoPanelVar)
                            .addGroup(VariablespanelLayout.createSequentialGroup()
                                .addComponent(CVbox, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addGap(22, 22, 22)
                                .addComponent(CVbutton, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)))
                        .addGap(0, 0, Short.MAX_VALUE))
                    .addComponent(SVBox, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addContainerGap())
        );
        VariablespanelLayout.setVerticalGroup(
            VariablespanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, VariablespanelLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(InfoPanelVar)
                .addGap(82, 82, 82)
                .addGroup(VariablespanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(GenotypeBox, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(GenotypeText, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addComponent(SVBox)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jSeparator2, javax.swing.GroupLayout.PREFERRED_SIZE, 10, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(113, 113, 113)
                .addComponent(jSeparator1, javax.swing.GroupLayout.PREFERRED_SIZE, 10, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(VariablespanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(CVbutton, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(CVbox, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap())
        );

        ResponsesPanel.setBorder(javax.swing.BorderFactory.createBevelBorder(javax.swing.border.BevelBorder.RAISED));
        ResponsesPanel.setPreferredSize(new java.awt.Dimension(200, 400));

        jLabel13.setFont(new java.awt.Font("Tahoma", 1, 12)); // NOI18N
        jLabel13.setText("Response Variables");

        jLabel14.setFont(new java.awt.Font("Tahoma", 1, 11)); // NOI18N
        jLabel14.setText("Select one or more");

        ScrollVariables.setAutoscrolls(true);
        ScrollVariables.setPreferredSize(new java.awt.Dimension(178, 120));

        ListVariables.setFont(new java.awt.Font("Tahoma", 1, 11)); // NOI18N
        ListVariables.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                ListVariablesMouseClicked(evt);
            }
        });
        ScrollVariables.setViewportView(ListVariables);

        SelectResponses.setText("Select All");
        SelectResponses.setPreferredSize(new java.awt.Dimension(178, 25));
        SelectResponses.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                SelectResponsesActionPerformed(evt);
            }
        });

        jLabel15.setFont(new java.awt.Font("Tahoma", 1, 11)); // NOI18N
        jLabel15.setText("Selected");

        jScrollPane3.setPreferredSize(new java.awt.Dimension(178, 120));

        ChosenVariables.setFont(new java.awt.Font("Tahoma", 1, 11)); // NOI18N
        ChosenVariables.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                ChosenVariablesMouseClicked(evt);
            }
        });
        jScrollPane3.setViewportView(ChosenVariables);

        DropResponses.setText("Drop All");
        DropResponses.setPreferredSize(new java.awt.Dimension(178, 25));
        DropResponses.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                DropResponsesActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout ResponsesPanelLayout = new javax.swing.GroupLayout(ResponsesPanel);
        ResponsesPanel.setLayout(ResponsesPanelLayout);
        ResponsesPanelLayout.setHorizontalGroup(
            ResponsesPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(ResponsesPanelLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(ResponsesPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(ResponsesPanelLayout.createSequentialGroup()
                        .addComponent(SelectResponses, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addGap(0, 0, Short.MAX_VALUE))
                    .addGroup(ResponsesPanelLayout.createSequentialGroup()
                        .addGroup(ResponsesPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jLabel13, javax.swing.GroupLayout.PREFERRED_SIZE, 138, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addComponent(ScrollVariables, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addComponent(jLabel14)
                            .addComponent(jLabel15)
                            .addComponent(jScrollPane3, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addComponent(DropResponses, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                        .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))))
        );
        ResponsesPanelLayout.setVerticalGroup(
            ResponsesPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(ResponsesPanelLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jLabel13)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jLabel14)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(ScrollVariables, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(SelectResponses, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addComponent(jLabel15)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jScrollPane3, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(DropResponses, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap())
        );

        Ran_Eff.setBorder(javax.swing.BorderFactory.createBevelBorder(javax.swing.border.BevelBorder.RAISED));
        Ran_Eff.setPreferredSize(new java.awt.Dimension(200, 400));

        jLabel11.setFont(new java.awt.Font("Tahoma", 1, 12)); // NOI18N
        jLabel11.setText("Random effects:");

        jLabel22.setFont(new java.awt.Font("Tahoma", 1, 11)); // NOI18N
        jLabel22.setText("Select one or more");

        ScrollVariables2.setPreferredSize(new java.awt.Dimension(178, 120));

        ListRandom.setFont(new java.awt.Font("Tahoma", 1, 11)); // NOI18N
        ListRandom.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                ListRandomMouseClicked(evt);
            }
        });
        ScrollVariables2.setViewportView(ListRandom);

        SelectRandom.setText("Select All");
        SelectRandom.setPreferredSize(new java.awt.Dimension(178, 25));
        SelectRandom.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                SelectRandomActionPerformed(evt);
            }
        });

        jLabel21.setFont(new java.awt.Font("Tahoma", 1, 11)); // NOI18N
        jLabel21.setText("Selected");

        jScrollPane5.setPreferredSize(new java.awt.Dimension(178, 120));

        ChosenRandom.setFont(new java.awt.Font("Tahoma", 1, 11)); // NOI18N
        ChosenRandom.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                ChosenRandomMouseClicked(evt);
            }
        });
        jScrollPane5.setViewportView(ChosenRandom);

        DropRandom.setText("Drop All");
        DropRandom.setPreferredSize(new java.awt.Dimension(178, 25));
        DropRandom.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                DropRandomActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout Ran_EffLayout = new javax.swing.GroupLayout(Ran_Eff);
        Ran_Eff.setLayout(Ran_EffLayout);
        Ran_EffLayout.setHorizontalGroup(
            Ran_EffLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(Ran_EffLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(Ran_EffLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jLabel11)
                    .addComponent(jLabel22)
                    .addComponent(DropRandom, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jScrollPane5, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel21)
                    .addComponent(ScrollVariables2, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(SelectRandom, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
        Ran_EffLayout.setVerticalGroup(
            Ran_EffLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(Ran_EffLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jLabel11)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jLabel22)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(ScrollVariables2, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(SelectRandom, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addComponent(jLabel21)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jScrollPane5, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(DropRandom, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap())
        );

        Fix_Eff.setBorder(javax.swing.BorderFactory.createBevelBorder(javax.swing.border.BevelBorder.RAISED));
        Fix_Eff.setPreferredSize(new java.awt.Dimension(200, 400));

        jLabel12.setFont(new java.awt.Font("Tahoma", 1, 12)); // NOI18N
        jLabel12.setText("Fixed effects:");

        jLabel29.setFont(new java.awt.Font("Tahoma", 1, 11)); // NOI18N
        jLabel29.setText("Select one or more");

        ScrollVariables3.setPreferredSize(new java.awt.Dimension(178, 120));

        ListFixed.setFont(new java.awt.Font("Tahoma", 1, 11)); // NOI18N
        ListFixed.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                ListFixedMouseClicked(evt);
            }
        });
        ScrollVariables3.setViewportView(ListFixed);

        SelectFixed.setText("Select All");
        SelectFixed.setPreferredSize(new java.awt.Dimension(178, 25));
        SelectFixed.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                SelectFixedActionPerformed(evt);
            }
        });

        jLabel28.setFont(new java.awt.Font("Tahoma", 1, 11)); // NOI18N
        jLabel28.setText("Selected");

        jScrollPane6.setPreferredSize(new java.awt.Dimension(178, 120));

        ChosenFixed.setFont(new java.awt.Font("Tahoma", 1, 11)); // NOI18N
        ChosenFixed.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                ChosenFixedMouseClicked(evt);
            }
        });
        jScrollPane6.setViewportView(ChosenFixed);

        DropFixed.setText("Drop All");
        DropFixed.setPreferredSize(new java.awt.Dimension(178, 25));
        DropFixed.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                DropFixedActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout Fix_EffLayout = new javax.swing.GroupLayout(Fix_Eff);
        Fix_Eff.setLayout(Fix_EffLayout);
        Fix_EffLayout.setHorizontalGroup(
            Fix_EffLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(Fix_EffLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(Fix_EffLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jLabel12)
                    .addComponent(jLabel29)
                    .addComponent(DropFixed, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jScrollPane6, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel28)
                    .addComponent(ScrollVariables3, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(SelectFixed, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
        Fix_EffLayout.setVerticalGroup(
            Fix_EffLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(Fix_EffLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jLabel12)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jLabel29)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(ScrollVariables3, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(SelectFixed, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addComponent(jLabel28)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jScrollPane6, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(DropFixed, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap())
        );

        javax.swing.GroupLayout BigPanelLayout = new javax.swing.GroupLayout(BigPanel);
        BigPanel.setLayout(BigPanelLayout);
        BigPanelLayout.setHorizontalGroup(
            BigPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, BigPanelLayout.createSequentialGroup()
                .addComponent(Methodpanel, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(Variablespanel, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(ResponsesPanel, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(Ran_Eff, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(Fix_Eff, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
        BigPanelLayout.setVerticalGroup(
            BigPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, BigPanelLayout.createSequentialGroup()
                .addGroup(BigPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(Fix_Eff, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.PREFERRED_SIZE, 406, Short.MAX_VALUE)
                    .addComponent(Ran_Eff, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.PREFERRED_SIZE, 406, Short.MAX_VALUE)
                    .addComponent(ResponsesPanel, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.PREFERRED_SIZE, 406, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(Methodpanel, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(Variablespanel, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addGap(3, 3, 3))
        );

        InfoPanel.setBorder(javax.swing.BorderFactory.createBevelBorder(javax.swing.border.BevelBorder.RAISED));
        InfoPanel.setCursor(new java.awt.Cursor(java.awt.Cursor.DEFAULT_CURSOR));
        InfoPanel.setInheritsPopupMenu(true);
        InfoPanel.setOpaque(false);
        InfoPanel.addPropertyChangeListener(new java.beans.PropertyChangeListener() {
            public void propertyChange(java.beans.PropertyChangeEvent evt) {
                InfoPanelPropertyChange(evt);
            }
        });

        Info.setEditable(false);
        Info.setColumns(20);
        Info.setFont(new java.awt.Font("Tahoma", 0, 11)); // NOI18N
        Info.setRows(5);
        Info.setText("     \n     Welcome to lme4GS (lme for Genomic Selection). Version 1.0 (2019-06-25)\n     Copyright © 2016 Centro Internacional de Mejoramiento de Maíz y Trigo (CIMMYT).\n     \n     \n     Authors:\n     Nallely Bautista\n     Paulino Pérez\n     Francisco Rodríguez\n     Angela Pacheco\n     Juan Burgueño\n     Gregorio Alvarado\n     José Crossa\n     \n     \n     This program is based in some components from Java, developed by ORACLE AMERICA, INC. and R, developed by Kurt Hornik. Any Java \n     component of this program is hereby licensed under the Oracle Binary Code License Agreement for the Java SE Platform Products and \n     JavaFX made available by Oracle (available at http://www.oracle.com/technetwork/java/javase/terms/license/index.html). Any R\n     component of this program as well as the program as a whole developed by CIMMYT are hereby licensed as per the terms of the GNU\n     General Public License version 3(available at http://www.gnu.org/licenses/gpl-3.0.html), as specified below.\n\n     This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published\n     by the Free Software Foundation; either version 3 of the License, or any later version.\n\n     This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of\n     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.\n\n     You should have received a copy of the GNU General Public License along with this program; if not, write to the Free Software Foundation, Inc., \n     51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.\n\n     For further information, please contact CIMMYT at  CIMMYT-Knowledge-Center@cgiar.org or at Km. 45 Carretera Mexico-Veracruz, El Batán,\n     Texcoco, Estado de México, México, C.P. 56237.\n\n     We have invested a lot of time and effort in creating lme4GS, please cite it when using it for data analysis.\n");
        InfoPanel.setViewportView(Info);

        jScrollPane1.setBackground(new java.awt.Color(204, 204, 204));
        jScrollPane1.setAutoscrolls(true);
        jScrollPane1.setInheritsPopupMenu(true);
        jScrollPane1.setOpaque(false);
        jScrollPane1.setPreferredSize(new java.awt.Dimension(2874, 3767));

        PanelMenus.setBackground(new java.awt.Color(240, 240, 240));
        PanelMenus.setBorder(javax.swing.BorderFactory.createEtchedBorder());
        PanelMenus.setModel(new javax.swing.table.DefaultTableModel(
            new Object [][] {
                {},
                {},
                {},
                {}
            },
            new String [] {

            }
        ));
        PanelMenus.setMaximumSize(new java.awt.Dimension(32767, 32767));
        PanelMenus.setMinimumSize(new java.awt.Dimension(23, 23));
        jScrollPane1.setViewportView(PanelMenus);

        ResultsPanel.setBorder(javax.swing.BorderFactory.createBevelBorder(javax.swing.border.BevelBorder.RAISED));

        jLabel19.setFont(new java.awt.Font("Tahoma", 1, 12)); // NOI18N
        jLabel19.setText("Files saved in output folder");

        TreeResults.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                TreeResultsMouseClicked(evt);
            }
        });
        jScrollPane2.setViewportView(TreeResults);

        javax.swing.GroupLayout ResultsPanelLayout = new javax.swing.GroupLayout(ResultsPanel);
        ResultsPanel.setLayout(ResultsPanelLayout);
        ResultsPanelLayout.setHorizontalGroup(
            ResultsPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(ResultsPanelLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(ResultsPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(ResultsPanelLayout.createSequentialGroup()
                        .addComponent(jLabel19)
                        .addGap(0, 311, Short.MAX_VALUE))
                    .addComponent(jScrollPane2))
                .addContainerGap())
        );
        ResultsPanelLayout.setVerticalGroup(
            ResultsPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(ResultsPanelLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jLabel19)
                .addGap(18, 18, 18)
                .addComponent(jScrollPane2, javax.swing.GroupLayout.PREFERRED_SIZE, 23, Short.MAX_VALUE)
                .addContainerGap())
        );

        PanelBar.setBackground(new java.awt.Color(255, 255, 255));
        PanelBar.setBorder(javax.swing.BorderFactory.createBevelBorder(javax.swing.border.BevelBorder.RAISED));
        PanelBar.setPreferredSize(new java.awt.Dimension(611, 40));

        Imputing.setFont(new java.awt.Font("Tahoma", 1, 12)); // NOI18N
        Imputing.setForeground(new java.awt.Color(255, 255, 255));
        Imputing.setText("Markers Quality...");

        Recoding.setFont(new java.awt.Font("Tahoma", 1, 12)); // NOI18N
        Recoding.setForeground(new java.awt.Color(255, 255, 255));
        Recoding.setText("Recoding...");

        Loading.setFont(new java.awt.Font("Tahoma", 1, 12)); // NOI18N
        Loading.setForeground(new java.awt.Color(255, 255, 255));
        Loading.setText("Loading...");

        Analyzing.setFont(new java.awt.Font("Tahoma", 1, 12)); // NOI18N
        Analyzing.setForeground(new java.awt.Color(255, 255, 255));
        Analyzing.setText("Analyzing...");

        Pbarra.setBackground(new java.awt.Color(204, 204, 255));
        Pbarra.setForeground(new java.awt.Color(255, 255, 255));
        Pbarra.setBorderPainted(false);
        Pbarra.setCursor(new java.awt.Cursor(java.awt.Cursor.DEFAULT_CURSOR));
        Pbarra.setPreferredSize(new java.awt.Dimension(500, 15));

        StopProcess.setActionCommand("X");
        StopProcess.setLabel("X");
        StopProcess.setPreferredSize(new java.awt.Dimension(15, 15));
        StopProcess.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                StopProcessActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout PanelBarLayout = new javax.swing.GroupLayout(PanelBar);
        PanelBar.setLayout(PanelBarLayout);
        PanelBarLayout.setHorizontalGroup(
            PanelBarLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, PanelBarLayout.createSequentialGroup()
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addComponent(Imputing)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(Recoding)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(Loading)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(Analyzing)
                .addGap(18, 18, 18)
                .addComponent(Pbarra, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(StopProcess, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap())
        );
        PanelBarLayout.setVerticalGroup(
            PanelBarLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(PanelBarLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(PanelBarLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(Pbarra, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(StopProcess, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addGroup(PanelBarLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                        .addComponent(Analyzing)
                        .addComponent(Loading)
                        .addComponent(Recoding)
                        .addComponent(Imputing)))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        javax.swing.GroupLayout jPanel2Layout = new javax.swing.GroupLayout(jPanel2);
        jPanel2.setLayout(jPanel2Layout);
        jPanel2Layout.setHorizontalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addGroup(jPanel2Layout.createSequentialGroup()
                        .addComponent(jScrollPane1, javax.swing.GroupLayout.PREFERRED_SIZE, 0, Short.MAX_VALUE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(ResultsPanel, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                    .addComponent(BigPanel, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(InfoPanel, javax.swing.GroupLayout.PREFERRED_SIZE, 25, Short.MAX_VALUE)
                .addContainerGap())
            .addComponent(PanelBar, javax.swing.GroupLayout.DEFAULT_SIZE, 1251, Short.MAX_VALUE)
            .addComponent(jPanel1, javax.swing.GroupLayout.DEFAULT_SIZE, 1251, Short.MAX_VALUE)
        );
        jPanel2Layout.setVerticalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addComponent(jPanel1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel2Layout.createSequentialGroup()
                        .addComponent(BigPanel, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(ResultsPanel, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                            .addComponent(jScrollPane1, javax.swing.GroupLayout.PREFERRED_SIZE, 0, Short.MAX_VALUE)))
                    .addComponent(InfoPanel))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(PanelBar, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
        );

        jScrollPane7.setViewportView(jPanel2);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jScrollPane7, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jScrollPane7, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
        );

        pack();
    }// </editor-fold>//GEN-END:initComponents

    @SuppressWarnings("unchecked")
    private void OpenActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_OpenActionPerformed
        // TODO add your handling code here:
        
        FileFilter ft = new FileNameExtensionFilter("Text Files", "txt", "csv", "CSV", "xlsx");
        db.addChoosableFileFilter( ft );
        int returnVal;
        mainPath = System.getProperty("user.dir");
        if(Examples==0){
            returnVal = db.showOpenDialog( this );
            ExamplesMark=0;
            ExamplesPed=0;
            ExamplesCVSets=0;
        }else{
            File initialfile = db.getCurrentDirectory();
            File Examplesfile = new File(mainPath + "\\Examples");
            db.setCurrentDirectory(Examplesfile);
            returnVal = db.showOpenDialog( this );
            db.setCurrentDirectory(initialfile);
            Examples=0;
        }
        
        
        
        if(returnVal == javax.swing.JFileChooser.APPROVE_OPTION) { 
            SitesTo.setVisible(false);
            ListSites.setModel(new DefaultListModel());
            ChosenSites.setModel(new DefaultListModel());
            ListVariables.setModel(new DefaultListModel());
            ChosenVariables.setModel(new DefaultListModel());
            ListRandom.setModel(new DefaultListModel());
            ChosenRandom.setModel(new DefaultListModel());
            ListFixed.setModel(new DefaultListModel());
            ChosenFixed.setModel(new DefaultListModel());
            modelMarkers = null;
            modelPed = null;
            modelList.clear();
            DesignBox.setSelectedIndex(0);
            Markername.setText("");
            Pedigreename.setText("");
            java.io.File file = db.getSelectedFile( );
            filedir = file.toString( );
            int result, result2, result21, result3;
            //String[] colNames=null;
            result = filedir.indexOf( ".txt" );
            result2 = filedir.indexOf( ".csv" );
            result21 = filedir.indexOf( ".CSV" );
            result3 = filedir.indexOf( ".xlsx" );
            if(result != -1 || result2 != -1 || result21 != -1 || result3 != -1){
                worktodo=1;
                taskInfinite=new ProgressInfinite();
                taskInfinite.execute();              
            }else{
                JOptionPane.showMessageDialog(this, "Incorrect file, it must be .csv, .CSV, .txt or .xlsx");
            }
        }
    }//GEN-LAST:event_OpenActionPerformed

    private void AnalyzeActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_AnalyzeActionPerformed
        // Do analysis:
        if("".equals(filedir)){
            JOptionPane.showMessageDialog(this, "No file chosen");
        }else{
            re.eval("library(BGLR)");
            re.eval("library(MASS)");
            re.eval("library(lme4)");
            
            

            ListModel Responses = ChosenVariables.getModel();
            ListModel Randoms = ChosenRandom.getModel();
            ListModel Fixeds = ChosenFixed.getModel();
            //String Blk = (String) BlkBox.getSelectedItem();
            //String Rep = (String) RepBox.getSelectedItem();
            String Gen = (String) GenotypeBox.getSelectedItem();
            //re.assign("Rep",Rep);
            //re.assign("Blk",Blk);
            re.assign("Gen",Gen);
            //ListModel Sites = ChosenSites.getModel();
            int run = 0;  // 0 run analisis, 1 no corre analisis
              
                if("Choose variable".equals(Gen)){
                    JOptionPane.showMessageDialog(this, "No Genotype variable chosen");
                    run = 1;
                }else if((MarkerBox.isSelected()) && "".equals(Markername.getText())){
                    JOptionPane.showMessageDialog(this, "You must load a Markers file");
                    run = 1; 
                }else if(!MarkerBox.isSelected() && !RelationBox.isSelected() && !RandomBox.isSelected() && !FixedBox.isSelected()){
                    JOptionPane.showMessageDialog(this, "You must choose at least one predictor option:" + "\n\n" +
                        "Markers" + "\n" + "Pedigree" + "\n" + "Random effects" + "\n" + "Fixed effects" + "\n");
                    run = 1;
                }else if(RelationBox.isSelected() && "".equals(Pedigreename.getText())){
                    JOptionPane.showMessageDialog(this, "You must load a Pedigree file");
                    run = 1;
                }else if(RandomBox.isSelected() && Randoms.getSize()==0){
                    JOptionPane.showMessageDialog(this, "You must choose at least one random effect");
                    run = 1;
                }else if(FixedBox.isSelected() && Fixeds.getSize()==0){
                    JOptionPane.showMessageDialog(this, "You must choose at least one fixed effect");
                    run = 1;
                }
                
            
            if(Responses.getSize()==0){
                JOptionPane.showMessageDialog(this, "No Response variable chosen");
            }else if(run==0){
                wasstop=1;
                worktodo=5;
                /*if(ExperimentalDesignBox.isSelected() & !"One site".equals(Site)){
                    re.assign("sitiosAnalizar",Sites.toString());
                    re.eval("sitiosAnalizar <- unlist(strsplit(unlist(strsplit(unlist(strsplit(sitiosAnalizar,split=', ')),split='[[]'))[-1],split=\"\\\\]\"))");                   
                    re.eval("datosOriginal <- datos");
                    re.eval("datos <- DatosSites(datos,Site,sitiosAnalizar)");
                }*/
                taskInfinite=new ProgressInfinite();
                taskInfinite.execute();
                /*if(ExperimentalDesignBox.isSelected() & !"One site".equals(Site)){                  
                    re.eval("datos <- datosOriginal");
                    re.eval("rm(datosOriginal)");
                }*/
            }
        }
    }//GEN-LAST:event_AnalyzeActionPerformed

    private void HelpActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_HelpActionPerformed
        Object[] options = {"User's Manual", "License","Examples"};
        int n = JOptionPane.showOptionDialog(this,
            "What do you want to see?",
            "Help",
            JOptionPane.DEFAULT_OPTION,
            JOptionPane.PLAIN_MESSAGE,
            null,
            options,
            options[0]);
        mainPath = System.getProperty("user.dir");
        if(n==0){
            File Manual = new File(mainPath + "\\Manuals" + "\\BGLR User Manual.pdf");
            try {
                Desktop.getDesktop().open(Manual);
            } catch (IOException ex) {
                Logger.getLogger(BGLR.class.getName()).log(Level.SEVERE, null, ex);
            }
        } else if(n==1){
            if(about == null){
                System.out.println("Creating window about");
                about = new About();
                about.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
            }else{
                System.out.println("Setting visible true");
                about.setVisible(true);
            }
        }else if(n == 2){
            Examples=1;
            ExamplesMark=1;
            ExamplesPed=1;
            ExamplesCVSets=1;
            Open.doClick();
        }
    }//GEN-LAST:event_HelpActionPerformed

    @SuppressWarnings("unchecked")
    private void RepBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_RepBoxActionPerformed
        // TODO add your handling code here:
        DefaultListModel modelList2 = new DefaultListModel();
        for(int i=0; i < modelList.size(); i++) {
            modelList2.add(i, modelList.toArray()[i]);
        }

        if(RepBox.getSelectedItem()!="Choose variable"){
            modelList2.removeElement(RepBox.getSelectedItem());
        }
        modelList2.removeElement(BlkBox.getSelectedItem());
        modelList2.removeElement(GenotypeBox.getSelectedItem());
        ListVariables.setModel(modelList2);
        ListFixed.setModel(modelList2);
        if(ChosenVariables.getModel().getSize()!=0){
            modelList2=(DefaultListModel) ChosenVariables.getModel();
            int indexofelement = modelList2.indexOf(RepBox.getSelectedItem());
            if(indexofelement!=-1){
                modelList2.removeElementAt(indexofelement);
                ChosenVariables.setModel(modelList2);
            }
        }
        if(ChosenFixed.getModel().getSize()!=0){
            modelList2=(DefaultListModel) ChosenFixed.getModel();
            int indexofelement = modelList2.indexOf(RepBox.getSelectedItem());
            if(indexofelement!=-1){
                modelList2.removeElementAt(indexofelement);
                ChosenFixed.setModel(modelList2);
            }
        }
    }//GEN-LAST:event_RepBoxActionPerformed

    @SuppressWarnings("unchecked")
    private void BlkBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_BlkBoxActionPerformed
        // TODO add your handling code here:
        DefaultListModel modelList2 = new DefaultListModel();
        for(int i=0; i < modelList.size(); i++) {
            modelList2.add(i, modelList.toArray()[i]);
        }
        modelList2.removeElement(RepBox.getSelectedItem());
        if(BlkBox.getSelectedItem()!="null" & BlkBox.getSelectedItem()!="Choose variable"){
            modelList2.removeElement(BlkBox.getSelectedItem());
        }
        modelList2.removeElement(GenotypeBox.getSelectedItem());
        ListVariables.setModel(modelList2);
        ListFixed.setModel(modelList2);
        if(ChosenVariables.getModel().getSize()!=0){
            modelList2=(DefaultListModel) ChosenVariables.getModel();
            int indexofelement = modelList2.indexOf(BlkBox.getSelectedItem());
            if(indexofelement!=-1){
                modelList2.removeElementAt(indexofelement);
                ChosenVariables.setModel(modelList2);
            }
        }
        if(ChosenFixed.getModel().getSize()!=0){
            modelList2=(DefaultListModel) ChosenFixed.getModel();
            int indexofelement = modelList2.indexOf(BlkBox.getSelectedItem());
            if(indexofelement!=-1){
                modelList2.removeElementAt(indexofelement);
                ChosenFixed.setModel(modelList2);
            }
        }
    }//GEN-LAST:event_BlkBoxActionPerformed

    private void DesignBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_DesignBoxActionPerformed
        // TODO add your handling code here:
        if(DesignBox.getSelectedItem()=="RCBD"){
            BlkBox.setEnabled(false);
            BlkBox.setSelectedItem("Choose variable");
        }else{
            BlkBox.setEnabled(true);
            BlkBox.setSelectedItem("Choose variable");
        }
    }//GEN-LAST:event_DesignBoxActionPerformed

    @SuppressWarnings("unchecked")
    private void ListVariablesMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_ListVariablesMouseClicked
        // TODO add your handling code here:
        JList list = (JList)evt.getSource();
        if (evt.getClickCount() == 2) {
            DefaultListModel modelList2 = new DefaultListModel();
            int index = list.locationToIndex(evt.getPoint());
            if(ChosenVariables.getModel().getSize()==0){
                modelList2.add(0, ListVariables.getModel().getElementAt(index));
            }else{
                modelList2=(DefaultListModel) ChosenVariables.getModel();
                int indexofelement = modelList2.indexOf(ListVariables.getModel().getElementAt(index));
                if(indexofelement==-1){
                    modelList2.add(ChosenVariables.getModel().getSize(), ListVariables.getModel().getElementAt(index));
                }
            }
            ChosenVariables.setModel(modelList2);         
            DefaultListModel modelList3 = new DefaultListModel();          
            int j=0;
            for(int i=0; i < modelList.size(); i++) {
                int secondIndex = 0;
                boolean igual = false;
                while(secondIndex < modelList2.size()) {            
                    igual = modelList.toArray()[i]==modelList2.toArray()[secondIndex];
                    if(igual){
                        break;
                    }
                    secondIndex++;
                }
                if(!igual){
                    modelList3.add(j, modelList.toArray()[i]);
                    j++;
                } 
            }
            ListRandom.setModel(modelList3); 
            ListFixed.setModel(modelList3); 
            modelList3 = new DefaultListModel();
            DefaultListModel modelList4 = (DefaultListModel) ChosenRandom.getModel();
            j=0;
            for(int i=0; i < modelList4.size(); i++) {
                int secondIndex = 0;
                boolean igual = false;
                while(secondIndex < modelList2.size()) {            
                    igual = modelList4.toArray()[i]==modelList2.toArray()[secondIndex];
                    if(igual){
                        break;
                    }
                    secondIndex++;
                }
                if(!igual){
                    modelList3.add(j, modelList4.toArray()[i]);
                    j++;
                } 
            }
            ChosenRandom.setModel(modelList3);
            modelList3 = new DefaultListModel();
            modelList4 = (DefaultListModel) ChosenFixed.getModel();
            j=0;
            for(int i=0; i < modelList4.size(); i++) {
                int secondIndex = 0;
                boolean igual = false;
                while(secondIndex < modelList2.size()) {            
                    igual = modelList4.toArray()[i]==modelList2.toArray()[secondIndex];
                    if(igual){
                        break;
                    }
                    secondIndex++;
                }
                if(!igual){
                    modelList3.add(j, modelList4.toArray()[i]);
                    j++;
                } 
            }
            ChosenFixed.setModel(modelList3);
        }         
    }//GEN-LAST:event_ListVariablesMouseClicked

    @SuppressWarnings("unchecked")
    private void ChosenVariablesMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_ChosenVariablesMouseClicked
        // TODO add your handling code here:
        JList list = (JList)evt.getSource();
        if (evt.getClickCount() == 2) {
            DefaultListModel modelList2;// = new DefaultListModel();
            int index = list.locationToIndex(evt.getPoint());
            modelList2=(DefaultListModel) ChosenVariables.getModel();
            modelList2.removeElementAt(index);
            ChosenVariables.setModel(modelList2);
            DefaultListModel modelList3 = new DefaultListModel();          
            int j=0;
            for(int i=0; i < modelList.size(); i++) {
                int secondIndex = 0;
                boolean igual = false;
                while(secondIndex < modelList2.size()) {            
                    igual = modelList.toArray()[i]==modelList2.toArray()[secondIndex];
                    if(igual){
                        break;
                    }
                    secondIndex++;
                }
                if(!igual){
                    modelList3.add(j, modelList.toArray()[i]);
                    j++;
                } 
            }
            ListRandom.setModel(modelList3);  
            ListFixed.setModel(modelList3);
        }
    }//GEN-LAST:event_ChosenVariablesMouseClicked

    @SuppressWarnings("unchecked")
    private void SelectResponsesActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_SelectResponsesActionPerformed
        // TODO add your handling code here:
        DefaultListModel modelList2 = new DefaultListModel();
        for(int i=0; i < ListVariables.getModel().getSize(); i++) {
            modelList2.add(i,ListVariables.getModel().getElementAt(i));
        }
        ChosenVariables.setModel(modelList2);
        DefaultListModel modelList3 = new DefaultListModel();          
        int j=0;
        for(int i=0; i < modelList.size(); i++) {
            int secondIndex = 0;
            boolean igual = false;
            while(secondIndex < modelList2.size()) {            
                igual = modelList.toArray()[i]==modelList2.toArray()[secondIndex];
                if(igual){
                    break;
                }
                secondIndex++;
            }
            if(!igual){
                modelList3.add(j, modelList.toArray()[i]);
                j++;
            } 
        }
        ListRandom.setModel(modelList3); 
        ListFixed.setModel(modelList3); 
        modelList3 = new DefaultListModel();
        DefaultListModel modelList4 = (DefaultListModel) ChosenRandom.getModel();
        j=0;
        for(int i=0; i < modelList4.size(); i++) {
            int secondIndex = 0;
            boolean igual = false;
            while(secondIndex < modelList2.size()) {            
                igual = modelList4.toArray()[i]==modelList2.toArray()[secondIndex];
                if(igual){
                    break;
                }
                secondIndex++;
            }
            if(!igual){
                modelList3.add(j, modelList4.toArray()[i]);
                j++;
            } 
        }
        ChosenRandom.setModel(modelList3);
        ChosenFixed.setModel(modelList3);
    }//GEN-LAST:event_SelectResponsesActionPerformed

    @SuppressWarnings("unchecked")
    private void DropResponsesActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_DropResponsesActionPerformed
        // TODO add your handling code here:
        ChosenVariables.setModel(new DefaultListModel());
        ListRandom.setModel(modelList);
        ListFixed.setModel((DefaultListModel) ListVariables.getModel());
    }//GEN-LAST:event_DropResponsesActionPerformed

    @SuppressWarnings("unchecked")
    private void ListSitesMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_ListSitesMouseClicked
        // TODO add your handling code here:
        JList list = (JList)evt.getSource();
        if (evt.getClickCount() == 2) {
            DefaultListModel modelList2 = new DefaultListModel();
            int index = list.locationToIndex(evt.getPoint());
            if(ChosenSites.getModel().getSize()==0){
                modelList2.add(0, ListSites.getModel().getElementAt(index));
            }else{
                modelList2=(DefaultListModel) ChosenSites.getModel();
                int indexofelement = modelList2.indexOf(ListSites.getModel().getElementAt(index));
                if(indexofelement==-1){
                    modelList2.add(ChosenSites.getModel().getSize(), ListSites.getModel().getElementAt(index));
                }
            }
            ChosenSites.setModel(modelList2);
        }
    }//GEN-LAST:event_ListSitesMouseClicked

    @SuppressWarnings("unchecked")
    private void ChosenSitesMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_ChosenSitesMouseClicked
        // TODO add your handling code here:
        JList list = (JList)evt.getSource();
        if (evt.getClickCount() == 2) {
            DefaultListModel modelList2;// = new DefaultListModel();
            int index = list.locationToIndex(evt.getPoint());
            modelList2=(DefaultListModel) ChosenSites.getModel();
            modelList2.removeElementAt(index);
            ChosenSites.setModel(modelList2);
        }
    }//GEN-LAST:event_ChosenSitesMouseClicked

    @SuppressWarnings("unchecked")
    private void SelectSitesActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_SelectSitesActionPerformed
        // TODO add your handling code here:
        DefaultListModel modelList2 = new DefaultListModel();
        for(int i=0; i < ListSites.getModel().getSize(); i++) {
            modelList2.add(i,ListSites.getModel().getElementAt(i));
        }
        ChosenSites.setModel(modelList2);
    }//GEN-LAST:event_SelectSitesActionPerformed

    @SuppressWarnings("unchecked")
    private void DropSitesActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_DropSitesActionPerformed
        // TODO add your handling code here:
        ChosenSites.setModel(new DefaultListModel());
    }//GEN-LAST:event_DropSitesActionPerformed

    private void TreeResultsMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_TreeResultsMouseClicked
        // TODO add your handling code here:
        if (evt.getClickCount() == 2){
            TreePath tp = TreeResults.getSelectionPath();
            String path0="";
            if(tp.getPathCount()==3) path0=tp.getPathComponent(1)+"//"+tp.getLastPathComponent().toString();
            if(tp.getPathCount()==2)  path0=tp.getLastPathComponent().toString();
            //String nameFile = pathname+"Output_BGLR//"+OutputFolder.getText()+"//"+path0;
            //actualOutputDir

            String nameFile = actualOutputDir +"//"+path0;
            File file2=new File(nameFile);
            try {
                //Runtime.getRuntime().exec(new String[] {"cmd.exe", "/c", nameFile});
                Desktop.getDesktop().open(file2);
            } catch (IOException ex) {
                Logger.getLogger(BGLR.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
    }//GEN-LAST:event_TreeResultsMouseClicked

    private void OutputFolderActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_OutputFolderActionPerformed
        // TODO add your handling code here:
    }//GEN-LAST:event_OutputFolderActionPerformed

    private void BGLRMarkerActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_BGLRMarkerActionPerformed
        // TODO add your handling code here:
    }//GEN-LAST:event_BGLRMarkerActionPerformed

    private void StopProcessActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_StopProcessActionPerformed
        // TODO add your handling code here:
        Object[] options = {"Stop Anlysis", "Continue"};
        int n = JOptionPane.showOptionDialog(this,
            "What do you want to do?",
            "Stop Analysis",
            JOptionPane.YES_NO_OPTION,
            JOptionPane.QUESTION_MESSAGE,
            null,
            options,
            options[0]);
        if(n==JOptionPane.YES_OPTION){
            wasstop = re.rniStop(0);
        }
    }//GEN-LAST:event_StopProcessActionPerformed

    private void LoadPedigreeActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_LoadPedigreeActionPerformed
        // TODO add your handling code here:
        //FileFilter ft = new FileNameExtensionFilter("Text Files", "txt", "csv", "CSV", "xlsx");
        FileFilter ft = new FileNameExtensionFilter("csv", "CSV");
        db.addChoosableFileFilter( ft );
        int returnVal;
        if(ExamplesPed==0){
            returnVal = db.showOpenDialog( this );
        }else{
            File initialfile = db.getCurrentDirectory();
            File Examplesfile = new File(mainPath + "\\Examples");
            db.setCurrentDirectory(Examplesfile);
            returnVal = db.showOpenDialog( this );
            db.setCurrentDirectory(initialfile);
        }
        if(returnVal == javax.swing.JFileChooser.APPROVE_OPTION) {          
            java.io.File file = db.getSelectedFile( );
            filedir = file.toString( );
            int result, result1; //, result2, result3;
            result = filedir.indexOf( ".csv" );
            result1 = filedir.indexOf( ".CSV" );
            //result2 = filedir.indexOf( ".txt" );
            //result3 = filedir.indexOf( ".xlsx" );
            if(result != -1 || result1 != -1){ // || result2 != -1 || result3 != -1){  
                worktodo=3;
                taskInfinite=new ProgressInfinite();
                taskInfinite.execute();
            }else{
                JOptionPane.showMessageDialog(this, "Incorrect file, it must be .csv or .CSV");
            }
        }
    }//GEN-LAST:event_LoadPedigreeActionPerformed

    private void InfoPanelPropertyChange(java.beans.PropertyChangeEvent evt) {//GEN-FIRST:event_InfoPanelPropertyChange
        // TODO add your handling code here:
    }//GEN-LAST:event_InfoPanelPropertyChange

    private void MarkerBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_MarkerBoxActionPerformed
        // TODO add your handling code here:
        if(MarkerBox.isSelected()){
            BGLRMarker.setEnabled(true);
            jLabel3.setVisible(false);
            BGLRMarker.setVisible(false);
            LoadMarker.setVisible(true);
            SeeMarker.setVisible(true);
            Markername.setVisible(true);
        }else if(!MarkerBox.isSelected()){
            jLabel3.setVisible(false);
            BGLRMarker.setVisible(false);
            LoadMarker.setVisible(false);
            SeeMarker.setVisible(false);
            Markername.setVisible(false);
        }
    }//GEN-LAST:event_MarkerBoxActionPerformed

    private void RelationBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_RelationBoxActionPerformed
        // TODO add your handling code here:
        if(RelationBox.isSelected()){
            LoadPedigree.setVisible(true);
            SeePedigree.setVisible(true);
            Pedigreename.setVisible(true);
        }else if(!RelationBox.isSelected()){
            LoadPedigree.setVisible(false);
            SeePedigree.setVisible(false);
            Pedigreename.setVisible(false);
        }
    }//GEN-LAST:event_RelationBoxActionPerformed

    private void RandomBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_RandomBoxActionPerformed
        // TODO add your handling code here:
        if(RandomBox.isSelected()){
            Ran_Eff.setVisible(true);
        }else if(!RandomBox.isSelected()){
            Ran_Eff.setVisible(false);
        }
    }//GEN-LAST:event_RandomBoxActionPerformed

    @SuppressWarnings("unchecked")
    private void ListRandomMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_ListRandomMouseClicked
        // TODO add your handling code here:
        JList list = (JList)evt.getSource();
        if (evt.getClickCount() == 2) {
            DefaultListModel modelList2 = new DefaultListModel();
            int index = list.locationToIndex(evt.getPoint());
            if(ChosenRandom.getModel().getSize()==0){
                modelList2.add(0, ListRandom.getModel().getElementAt(index));
            }else{
                modelList2=(DefaultListModel) ChosenRandom.getModel();
                int indexofelement = modelList2.indexOf(ListRandom.getModel().getElementAt(index));
                if(indexofelement==-1){
                    modelList2.add(ChosenRandom.getModel().getSize(), ListRandom.getModel().getElementAt(index));
                }
            }
            ChosenRandom.setModel(modelList2);
            if(ChosenFixed.getModel().getSize()!=0){            
                DefaultListModel modelList3 = new DefaultListModel();
                DefaultListModel modelList4 = (DefaultListModel) ChosenFixed.getModel();
                int j=0;
                for(int i=0; i < modelList4.size(); i++) {
                    int secondIndex = 0;
                    boolean igual = false;
                    while(secondIndex < modelList2.size()) {            
                        igual = modelList4.toArray()[i]==modelList2.toArray()[secondIndex];
                        if(igual){
                            break;
                        }
                        secondIndex++;
                    }
                    if(!igual){
                        modelList3.add(j, modelList4.toArray()[i]);
                        j++;
                    } 
                }
                ChosenFixed.setModel(modelList3);
            }           
        }
    }//GEN-LAST:event_ListRandomMouseClicked

    @SuppressWarnings("unchecked")
    private void ChosenRandomMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_ChosenRandomMouseClicked
        // TODO add your handling code here:
        JList list = (JList)evt.getSource();
        if (evt.getClickCount() == 2) {
            DefaultListModel modelList2;// = new DefaultListModel();
            int index = list.locationToIndex(evt.getPoint());
            modelList2=(DefaultListModel) ChosenRandom.getModel();
            modelList2.removeElementAt(index);
            ChosenRandom.setModel(modelList2);
        }    
    }//GEN-LAST:event_ChosenRandomMouseClicked

    @SuppressWarnings("unchecked")
    private void SelectRandomActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_SelectRandomActionPerformed
        // TODO add your handling code here:
        DefaultListModel modelList2 = new DefaultListModel();
        for(int i=0; i < ListRandom.getModel().getSize(); i++) {
            modelList2.add(i,ListRandom.getModel().getElementAt(i));
        }
        ChosenRandom.setModel(modelList2);
        ChosenFixed.setModel(new DefaultListModel());
    }//GEN-LAST:event_SelectRandomActionPerformed

    @SuppressWarnings("unchecked")
    private void DropRandomActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_DropRandomActionPerformed
        // TODO add your handling code here:
        ChosenRandom.setModel(new DefaultListModel());
    }//GEN-LAST:event_DropRandomActionPerformed

    private void SeeMarkerActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_SeeMarkerActionPerformed
        // TODO add your handling code here:
        if(null==modelMarkers){
            JOptionPane.showMessageDialog(this, "No Markers loaded");
        }else{
            markerview = new MarkersView(modelMarkers);
            markerview.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
            markerview.setVisible(true);
        }
    }//GEN-LAST:event_SeeMarkerActionPerformed

    private void LoadMarkerActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_LoadMarkerActionPerformed
        // TODO add your handling code here:
        //FileFilter ft = new FileNameExtensionFilter("Text Files", "txt", "csv", "CSV", "xlsx");
        FileFilter ft = new FileNameExtensionFilter("csv", "CSV");
        db.addChoosableFileFilter( ft );
        int returnVal;
        if(ExamplesMark==0){
            returnVal = db.showOpenDialog( this );
        }else{
            File initialfile = db.getCurrentDirectory();
            File Examplesfile = new File(mainPath + "\\Examples");
            db.setCurrentDirectory(Examplesfile);
            returnVal = db.showOpenDialog( this );
            db.setCurrentDirectory(initialfile);
        }
        if(returnVal == javax.swing.JFileChooser.APPROVE_OPTION) {          
            java.io.File file = db.getSelectedFile( );
            filedir = file.toString( );
            int result, result1; //, result2, result3;
            result = filedir.indexOf( ".csv" );
            result1 = filedir.indexOf( ".CSV" );
            //result2 = filedir.indexOf( ".txt" );
            //result3 = filedir.indexOf( ".xlsx" );
            if(result != -1 || result1 != -1){ // || result2 ! = -1 || result3 != -1){   
                worktodo=2;
                taskInfinite=new ProgressInfinite();
                taskInfinite.execute();
            }else{
                JOptionPane.showMessageDialog(this, "Incorrect file, it must be .csv or .CSV");
            }
        }
    }//GEN-LAST:event_LoadMarkerActionPerformed

    private void SeePedigreeActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_SeePedigreeActionPerformed
        // TODO add your handling code here:
        if(null==modelPed){
            JOptionPane.showMessageDialog(this, "No Relationship Matrix loaded");
        }else{
            pedview = new RelationshipView(modelPed);
            pedview.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
            pedview.setVisible(true); 
        }
    }//GEN-LAST:event_SeePedigreeActionPerformed

    private void PedigreenameActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_PedigreenameActionPerformed
        // TODO add your handling code here:
    }//GEN-LAST:event_PedigreenameActionPerformed

    private void MarkernameActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_MarkernameActionPerformed
        // TODO add your handling code here:
    }//GEN-LAST:event_MarkernameActionPerformed

    private void ExperimentalDesignBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_ExperimentalDesignBoxActionPerformed
        // TODO add your handling code here:
        if(ExperimentalDesignBox.isSelected()){
            VarText.setVisible(true);;
            RepText.setVisible(true);
            BlkText.setVisible(true);
            //GenotypeText.setVisible(true);
            //GenotypeBox.setVisible(true);
            DesignBox.setEnabled(true);

            RepBox.setVisible(true);
            BlkBox.setVisible(true);
            if(DesignBox.getSelectedItem()=="RCBD"){
                BlkBox.setEnabled(false);
            }else{
                BlkBox.setEnabled(true);
            }
        }else if(!ExperimentalDesignBox.isSelected()){
            VarText.setVisible(false);
            RepText.setVisible(false);
            BlkText.setVisible(false);
            //GenotypeText.setVisible(false);
            //GenotypeBox.setVisible(false);
            DesignBox.setEnabled(false);
            RepBox.setVisible(false);
            BlkBox.setVisible(false);
        }
    }//GEN-LAST:event_ExperimentalDesignBoxActionPerformed

    private void FixedBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_FixedBoxActionPerformed
        // TODO add your handling code here:
        if(FixedBox.isSelected()){
            Fix_Eff.setVisible(true);
        }else if(!FixedBox.isSelected()){
            Fix_Eff.setVisible(false);
        }   
    }//GEN-LAST:event_FixedBoxActionPerformed

    @SuppressWarnings("unchecked")
    private void ListFixedMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_ListFixedMouseClicked
        // TODO add your handling code here:
        JList list = (JList)evt.getSource();
        if (evt.getClickCount() == 2) {
            DefaultListModel modelList2 = new DefaultListModel();
            int index = list.locationToIndex(evt.getPoint());
            if(ChosenFixed.getModel().getSize()==0){
                modelList2.add(0, ListFixed.getModel().getElementAt(index));
            }else{
                modelList2=(DefaultListModel) ChosenFixed.getModel();
                int indexofelement = modelList2.indexOf(ListFixed.getModel().getElementAt(index));
                if(indexofelement==-1){
                    modelList2.add(ChosenFixed.getModel().getSize(), ListFixed.getModel().getElementAt(index));
                }
            }
            ChosenFixed.setModel(modelList2);
            if(ChosenRandom.getModel().getSize()!=0){            
                DefaultListModel modelList3 = new DefaultListModel();
                DefaultListModel modelList4 = (DefaultListModel) ChosenRandom.getModel();
                int j=0;
                for(int i=0; i < modelList4.size(); i++) {
                    int secondIndex = 0;
                    boolean igual = false;
                    while(secondIndex < modelList2.size()) {            
                        igual = modelList4.toArray()[i]==modelList2.toArray()[secondIndex];
                        if(igual){
                            break;
                        }
                        secondIndex++;
                    }
                    if(!igual){
                        modelList3.add(j, modelList4.toArray()[i]);
                        j++;
                    } 
                }
                ChosenRandom.setModel(modelList3);
            }  
        }
    }//GEN-LAST:event_ListFixedMouseClicked

    @SuppressWarnings("unchecked")
    private void SelectFixedActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_SelectFixedActionPerformed
        // TODO add your handling code here:
        DefaultListModel modelList2 = new DefaultListModel();
        for(int i=0; i < ListFixed.getModel().getSize(); i++) {
            modelList2.add(i,ListFixed.getModel().getElementAt(i));
        }
        ChosenFixed.setModel(modelList2);
        ChosenRandom.setModel(new DefaultListModel());
    }//GEN-LAST:event_SelectFixedActionPerformed

    @SuppressWarnings("unchecked")
    private void ChosenFixedMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_ChosenFixedMouseClicked
        // TODO add your handling code here:
         JList list = (JList)evt.getSource();
        if (evt.getClickCount() == 2) {
            DefaultListModel modelList2;// = new DefaultListModel();
            int index = list.locationToIndex(evt.getPoint());
            modelList2=(DefaultListModel) ChosenFixed.getModel();
            modelList2.removeElementAt(index);
            ChosenFixed.setModel(modelList2);
        } 
    }//GEN-LAST:event_ChosenFixedMouseClicked

    @SuppressWarnings("unchecked")
    private void DropFixedActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_DropFixedActionPerformed
        // TODO add your handling code here:
        ChosenFixed.setModel(new DefaultListModel());
    }//GEN-LAST:event_DropFixedActionPerformed

    private void OkCVActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_OkCVActionPerformed
        // TODO add your handling code here:
        CVModule.setVisible(false);
    }//GEN-LAST:event_OkCVActionPerformed

    private void CVbuttonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_CVbuttonActionPerformed
        // TODO add your handling code here:
        CVModule.setVisible(true);
    }//GEN-LAST:event_CVbuttonActionPerformed

    private void CVtypeActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_CVtypeActionPerformed
        // TODO add your handling code here:
        if(CVtype.getSelectedItem() == "Folds"){
            NumberFolds.setVisible(true);
            NumberFoldsLabel.setVisible(true);
            NumberCV.setVisible(false);
            NumberCVLabel.setVisible(false);
            TestingSize.setVisible(false);
            TestingSizeLabel.setVisible(false);
            CVSetname.setVisible(false);
            LoadCVSets.setVisible(false);
            SeeCVSets.setVisible(false);
        }else if(CVtype.getSelectedItem() == "Load Sets"){
            NumberFolds.setVisible(false);
            NumberFoldsLabel.setVisible(false);
            NumberCV.setVisible(false);
            NumberCVLabel.setVisible(false);
            TestingSize.setVisible(false);
            TestingSizeLabel.setVisible(false);
            CVSetname.setVisible(true);
            LoadCVSets.setVisible(true);
            SeeCVSets.setVisible(true);
        }else{
            NumberFolds.setVisible(false);
            NumberFoldsLabel.setVisible(false);
            NumberCV.setVisible(true);
            NumberCVLabel.setVisible(true);
            TestingSize.setVisible(true);
            TestingSizeLabel.setVisible(true);
            CVSetname.setVisible(false);
            LoadCVSets.setVisible(false);
            SeeCVSets.setVisible(false);
        }
    }//GEN-LAST:event_CVtypeActionPerformed

    private void LoadCVSetsActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_LoadCVSetsActionPerformed
        // TODO add your handling code here:
        //FileFilter ft = new FileNameExtensionFilter("Text Files", "txt", "csv","xlsx");
        FileFilter ft = new FileNameExtensionFilter("csv", "CSV");
        db.addChoosableFileFilter( ft );
        int returnVal;
        if(ExamplesCVSets==0){
            returnVal = db.showOpenDialog( this );
        }else{
            File initialfile = db.getCurrentDirectory();
            File Examplesfile = new File(mainPath + "\\Examples");
            db.setCurrentDirectory(Examplesfile);
            returnVal = db.showOpenDialog( this );
            db.setCurrentDirectory(initialfile);
        }
        if(returnVal == javax.swing.JFileChooser.APPROVE_OPTION) {          
            java.io.File file = db.getSelectedFile( );
            filedir = file.toString( );
            int result, result1; //, result2, result3;
            result = filedir.indexOf( ".csv" );
            result1 = filedir.indexOf( ".CSV" );
            //result2 = filedir.indexOf( ".txt" );
            //result3 = filedir.indexOf( ".xlsx" );
            if(result != -1 || result1 != -1){ // || result2 != -1 || result3 != -1){  
                worktodo=6;
                taskInfinite=new ProgressInfinite();
                taskInfinite.execute();
            }else{
                JOptionPane.showMessageDialog(this, "Incorrect file, it must be .csv or .CSV");
            }
        }
    }//GEN-LAST:event_LoadCVSetsActionPerformed

    private void CVSetnameActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_CVSetnameActionPerformed
        // TODO add your handling code here:
    }//GEN-LAST:event_CVSetnameActionPerformed

    private void SeeCVSetsActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_SeeCVSetsActionPerformed
        // TODO add your handling code here:
        if(null==modelCV){
            JOptionPane.showMessageDialog(this, "No Cross Validation Set loaded");
        }else{
            CVview = new CVView(modelCV);
            CVview.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
            CVview.setVisible(true);
        }
    }//GEN-LAST:event_SeeCVSetsActionPerformed

    private void CVboxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_CVboxActionPerformed
        // TODO add your handling code here:
        if(CVbox.isSelected()){
            CVbutton.setVisible(true);

        }else{
            CVbutton.setVisible(false);

        }
    }//GEN-LAST:event_CVboxActionPerformed

    private void SVBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_SVBoxActionPerformed
        // TODO add your handling code here:
    }//GEN-LAST:event_SVBoxActionPerformed

    @SuppressWarnings("unchecked")
    private void GenotypeBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_GenotypeBoxActionPerformed
        // TODO add your handling code here:
        DefaultListModel modelList2 = new DefaultListModel();
        for(int i=0; i < modelList.size(); i++) {
            modelList2.add(i, modelList.toArray()[i]);
        }

        modelList2.removeElement(RepBox.getSelectedItem());
        modelList2.removeElement(BlkBox.getSelectedItem());
        if(GenotypeBox.getSelectedItem()!="Choose variable"){
            modelList2.removeElement(GenotypeBox.getSelectedItem());
        }
        ListVariables.setModel(modelList2);
        ListFixed.setModel(modelList2);
        if(ChosenVariables.getModel().getSize()!=0){
            modelList2=(DefaultListModel) ChosenVariables.getModel();
            int indexofelement = modelList2.indexOf(GenotypeBox.getSelectedItem());
            if(indexofelement!=-1){
                modelList2.removeElementAt(indexofelement);
                ChosenVariables.setModel(modelList2);
            }
        }
        if(ChosenFixed.getModel().getSize()!=0){
            modelList2=(DefaultListModel) ChosenFixed.getModel();
            int indexofelement = modelList2.indexOf(GenotypeBox.getSelectedItem());
            if(indexofelement!=-1){
                modelList2.removeElementAt(indexofelement);
                ChosenFixed.setModel(modelList2);
            }
        }
    }//GEN-LAST:event_GenotypeBoxActionPerformed

    
    private static void loadVariables (){
        String version = System.getProperty("sun.arch.data.model");
        String r_home = "C:\\lme4GS\\BASE\\R-3.5\\";
        String jri_path;
        String r_path;
        if("64".equals(version)){    
            jri_path= "C:\\lme4GS\\BASE\\win-library\\3.5\\rJava\\jri\\x64\\";
            r_path= "C:\\lme4GS\\BASE\\R-3.5\\bin\\x64\\";
        }else{
            jri_path = "C:\\lme4GS\\BASE\\win-library\\3.5\\rJava\\jri\\i386\\";
            r_path = "C:\\lme4GS\\BASE\\R-3.5\\bin\\i386\\";
        }
        // create the process builder
        ProcessBuilder pb = new ProcessBuilder(); 
        pb.environment().put("R_HOME", r_home);
        pb.environment().put("Path", jri_path + ";" + r_path );
    }
    /**
     * @param args the command line arguments
     */
    public static void main(String args[]) {
       // loadVariables();
        /* Set the Nimbus look and feel */
        //<editor-fold defaultstate="collapsed" desc=" Look and feel setting code (optional) ">
        /* If Nimbus (introduced in Java SE 6) is not available, stay with the default look and feel.
         * For details see http://download.oracle.com/javase/tutorial/uiswing/lookandfeel/plaf.html 
         */
        try {
            for (javax.swing.UIManager.LookAndFeelInfo info : javax.swing.UIManager.getInstalledLookAndFeels()) {
                if ("Metal".equals(info.getName())) {
                    javax.swing.UIManager.setLookAndFeel(info.getClassName());
                    MetalLookAndFeel.setCurrentTheme(new OceanTheme());
                    break;
                }
            }
        } catch (ClassNotFoundException ex) {
            java.util.logging.Logger.getLogger(BGLR.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
        } catch (InstantiationException ex) {
            java.util.logging.Logger.getLogger(BGLR.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
        } catch (IllegalAccessException ex) {
            java.util.logging.Logger.getLogger(BGLR.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
        } catch (javax.swing.UnsupportedLookAndFeelException ex) {
            java.util.logging.Logger.getLogger(BGLR.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
        } catch (Exception ex){
            ex.printStackTrace();
        }
        //</editor-fold>

        /* Create and display the form */
        java.awt.EventQueue.invokeLater(new Runnable() {
            public void run() {
                try {
                    new BGLR().setVisible(true);
                } catch (Exception ex) {
                    Logger.getLogger(BGLR.class.getName()).log(Level.SEVERE, null, ex);
                }
            }
        });
    }

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton Analyze;
    private javax.swing.JLabel Analyzing;
    private javax.swing.JComboBox BGLRMarker;
    private javax.swing.JPanel BigPanel;
    private javax.swing.JPanel BigPanelCV;
    private javax.swing.JComboBox BlkBox;
    private javax.swing.JLabel BlkText;
    private javax.swing.JDialog CVModule;
    private javax.swing.JTextField CVSetname;
    private javax.swing.JCheckBox CVbox;
    private javax.swing.JButton CVbutton;
    private javax.swing.JPanel CVpanel;
    private javax.swing.JComboBox<String> CVtype;
    private javax.swing.JList ChosenFixed;
    private javax.swing.JList ChosenRandom;
    private javax.swing.JList ChosenSites;
    private javax.swing.JList ChosenVariables;
    private javax.swing.JComboBox DesignBox;
    private javax.swing.JButton DropFixed;
    private javax.swing.JButton DropRandom;
    private javax.swing.JButton DropResponses;
    private javax.swing.JButton DropSites;
    private javax.swing.JCheckBox ExperimentalDesignBox;
    private javax.swing.JPanel Fix_Eff;
    private javax.swing.JCheckBox FixedBox;
    private javax.swing.JComboBox GenotypeBox;
    private javax.swing.JLabel GenotypeText;
    private javax.swing.JButton Help;
    private javax.swing.JLabel Imputing;
    private javax.swing.JTextArea Info;
    private javax.swing.JScrollPane InfoPanel;
    private javax.swing.JLabel InfoPanelVar;
    private javax.swing.JList ListFixed;
    private javax.swing.JList ListRandom;
    private javax.swing.JList ListSites;
    private javax.swing.JList ListVariables;
    private javax.swing.JButton LoadCVSets;
    private javax.swing.JButton LoadMarker;
    private javax.swing.JButton LoadPedigree;
    private javax.swing.JLabel Loading;
    private javax.swing.JLabel LogoBSU;
    private javax.swing.JLabel LogoCIMMYT;
    private javax.swing.JCheckBox MarkerBox;
    private javax.swing.JTextField Markername;
    private javax.swing.JPanel Methodpanel;
    private javax.swing.JSpinner NumberCV;
    private javax.swing.JLabel NumberCVLabel;
    private javax.swing.JSpinner NumberFolds;
    private javax.swing.JLabel NumberFoldsLabel;
    private javax.swing.JButton OkCV;
    private javax.swing.JButton Open;
    private javax.swing.JTextField OutputFolder;
    private javax.swing.JPanel PanelBar;
    private javax.swing.JTable PanelMenus;
    private javax.swing.JProgressBar Pbarra;
    private javax.swing.JTextField Pedigreename;
    private javax.swing.JComboBox PlotFormatBox;
    private javax.swing.JPanel Ran_Eff;
    private javax.swing.JCheckBox RandomBox;
    private javax.swing.JLabel Recoding;
    private javax.swing.JCheckBox RelationBox;
    private javax.swing.JComboBox RepBox;
    private javax.swing.JLabel RepText;
    private javax.swing.JPanel ResponsesPanel;
    private javax.swing.JPanel ResultsPanel;
    private javax.swing.JCheckBox SVBox;
    private javax.swing.JScrollPane ScrollVariables;
    private javax.swing.JScrollPane ScrollVariables1;
    private javax.swing.JScrollPane ScrollVariables2;
    private javax.swing.JScrollPane ScrollVariables3;
    private javax.swing.JButton SeeCVSets;
    private javax.swing.JButton SeeMarker;
    private javax.swing.JButton SeePedigree;
    private javax.swing.JButton SelectFixed;
    private javax.swing.JButton SelectRandom;
    private javax.swing.JButton SelectResponses;
    private javax.swing.JButton SelectSites;
    private javax.swing.JPanel SitesTo;
    private java.awt.Button StopProcess;
    private javax.swing.JSpinner TestingSize;
    private javax.swing.JLabel TestingSizeLabel;
    private javax.swing.JTree TreeResults;
    private javax.swing.JLabel VarText;
    private javax.swing.JPanel Variablespanel;
    private javax.swing.JFileChooser db;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel11;
    private javax.swing.JLabel jLabel12;
    private javax.swing.JLabel jLabel13;
    private javax.swing.JLabel jLabel14;
    private javax.swing.JLabel jLabel15;
    private javax.swing.JLabel jLabel16;
    private javax.swing.JLabel jLabel17;
    private javax.swing.JLabel jLabel18;
    private javax.swing.JLabel jLabel19;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel21;
    private javax.swing.JLabel jLabel22;
    private javax.swing.JLabel jLabel23;
    private javax.swing.JLabel jLabel28;
    private javax.swing.JLabel jLabel29;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel31;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JScrollPane jScrollPane2;
    private javax.swing.JScrollPane jScrollPane3;
    private javax.swing.JScrollPane jScrollPane4;
    private javax.swing.JScrollPane jScrollPane5;
    private javax.swing.JScrollPane jScrollPane6;
    private javax.swing.JScrollPane jScrollPane7;
    private javax.swing.JSeparator jSeparator1;
    private javax.swing.JSeparator jSeparator2;
    private javax.swing.JSeparator jSeparator3;
    private javax.swing.JSeparator jSeparator4;
    private javax.swing.JSeparator jSeparator6;
    // End of variables declaration//GEN-END:variables
}
