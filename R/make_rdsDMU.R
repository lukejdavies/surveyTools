#' Function for generating standardised .rds DMU formats
#'
#' @description Takes a catalogue and various meta data/column information and writes out a standardised Data Management Unit (DMU) data product as a .rds file.
#'
#' @param DMUName Name for the DMU
#' @param cat data.frame of catalogue data to add to DMU
#' @param summary a 1/2 sentence overview description of your DMU
#' @param usrGen name of person who is making DMU
#' @param contactGen email contact of person who is making DMU
#' @param scriptGen script used to generate DMU
#' @param version version number identifier of script
#' @param coldescription a vector containing string descriptions of each column in cat
#' @param colucd a vector containing string Universal Content Descriptors (UCDs) for each column in cat (see http://cdsweb.u-strasbg.fr/UCD/tree/js/)
#' @param colunits a vector containing string units for each column in cat
#' @param README # a string README for the catalogue (please separate lines with \n
#' @param test TRUE/FLASE - If true catalogue can be run with dummy values (see example) and not give errors
#' @return a list containing the DMU. The DMU will also be written out as a .rds file with the format: DMUName,'_',format(Sys.time(), "%d_%m_%Y"),'_v',version,'.rds'
#' @author L. Davies <luke.j.davies@uwa.edu>
#' @examples
# DMUName<-'dummy'
# cat<-data.frame(col1=round(runif(10,1,10)), col2=runif(10,0,360), col3=runif(10,-90,90))
# summary<-'This is my catalogue, with things in it that I measured'
#usrGen<-'G. M. Bluth'
#contactGen<-'g.m.bluth@thebluthfamily.com'
#scriptGen<-'myScript.R'
#version<-0.1
#coldescription<-c('This is column1, it has column1-like things in it - maybe an ID', 'This is column2, supprisingly it has column2-like things in it - maybe an RA', 'This is column2, supprisingly it has column2-like things in it - maybe a DEC' )
#colucd<-c('meta.id', 'pos.eq.ra', 'pos.eq.dec')
#colunits<-c('none', 'deg', 'deg')
#README<-c('The is a README that describes this table. I can put in lots of information about hot the catalogue is generated and its providance. If I wane to start a new line, I should use \n. Or I can skip lines with \n\n. This way if someone wants to read it, they can easily do cat(DMU$README)')
#DMU<-make_rdsDMU(DMUName, cat, summary, usrGen, contactGen, scriptGen, version, coldescription,  colucd, colunits, README, test=TRUE)
#' @export
make_rdsDMU<-function(DMUName, cat, summary, usrGen, contactGen, scriptGen, version, coldescription,  colucd, colunits, README, add=NA, test=FALSE){

    cat(' \n \n****************************************\n')
    cat('****** GENERATING DMU FILE ******')
    cat(' \n****************************************\n \n')

    DMU<-list()
    DMU$cat<-cat
    DMU$meta$name<-DMUName
    DMU$meta$summary<-summary
    DMU$meta$usrGen<-usrGen
    DMU$meta$contactGen<-contactGen
    DMU$meta$scriptGen<-scriptGen
    DMU$meta$version<-version
    DMU$coldescription<-coldescription
    DMU$colucd<-colucd
    DMU$colunits<-colunits
    DMU$README<-README

    DMU$meta$dateGen<-date()
    DMU$colnames<-colnames(DMU$cat)
    DMU$meta$machineGen<-system('hostname', intern = TRUE)
    DMU$meta$RGen<-R.Version()

    numRows<-dim(DMU$cat)[1]
    numCols<-dim(DMU$cat)[2]


                                        # Perform tests that dummy values have been updated
    if (test==F){
        if (DMUName=='dummy'){
            cat('**WARNING** DMUFileName is still set to "dummy", please change \n')
            return(NULL)
        }
        if (DMU$meta$summary=='This is my catalogue, with things in it that I measured'){
            cat('**WARNING** DMU$meta$summary is still set to dummy value, please change \n')
            return(NULL)
        }
        if (DMU$meta$usrGen=='G. M. Bluth'){
            cat('**WARNING** DMU$meta$usrGen is still set to dummy value, please change \n')
            return(NULL)
        }
        if (DMU$meta$contactGen=='g.m.bluth@thebluthfamily.com'){
            cat('**WARNING** DMU$meta$contactGen is still set to dummy value, please change \n')
            return(NULL)
        }
        if (DMU$coldescription[1]=='This is column1, it has column1-like things in it - maybe an ID'){
            cat('**WARNING** DMU$meta$coldescription is still set to dummy value, please change \n')
            return(NULL)
        }
        if (DMU$README=='The is a README that describes this table. I can put in lots of information about hot the catalogue is generated and its providance. If I wane to start a new line, I should use \n. Or I can skip lines with \n\n. This way if someone wants to read it, they can easily do cat(DMU$README)'){
            cat('**WARNING** DMU$meta$README is still set to dummy value, please change \n')
            return(NULL)
        }
    }

                                        # Perform tests for size of column descriptor vectors
    if (length(DMU$coldescription)!=numCols){
        cat('DMU$coldescription does not have the correct length, please check \n')
        return(NULL)
    }else{
        cat('**CHECK PASSED** - coldescription has correct length \n \n')
    }

    if (length(DMU$colucd)!=numCols){
        cat('DMU$colucd does not have the correct length, please check \n')
        return(NULL)
    }else{
        cat('**CHECK PASSED** - colucd has correct length \n \n')
    }

    if (length(DMU$colunits)!=numCols){
        cat('DMU$colunits does not have the correct length, please check \n')
        return(NULL)
    }else{
        cat('**CHECK PASSED** - colunits has correct length \n \n')
    }

                                        # Perform verification check:

    cat(' \n \n You have generated a catalogue with ', numRows, ' rows and ', numCols, ' columns \n \n')
    cat('This catalogue has columns: \n')
    tmp<-data.frame(names=DMU$colnames, units=DMU$colunits,ucd=DMU$colucd, description=DMU$coldescription)
    print(tmp)
    cat('\n \n')
    cat('Your meta data is: \n')
    cat('name - ', DMUName, '\n')
    cat('summary - ', DMU$meta$summary, '\n')
    cat('usr - ', DMU$meta$usrGen, '\n')
    cat('contact - ', DMU$meta$contactGen, '\n')
    cat('script - ', DMU$meta$scriptGen, '\n')
    cat('version - ', DMU$meta$version, '\n')
    cat('date - ', DMU$meta$dateGen, '\n\n')

    if (!is.na(add)){
      DMU[[length(DMU)+1]]<-add
      names(DMU)[length(DMU)]<-'added'
    }

    fileName<-paste(DMUName,'_',format(Sys.time(), "%d_%m_%Y"),'_v',DMU$meta$version,'.rds',sep='')
    saveRDS(DMU, file=fileName)
    cat('***** Finished ***** \n Catalogue generated as: ', fileName, '\n')
    return(DMU)



}
