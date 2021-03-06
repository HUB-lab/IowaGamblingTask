function [decks] = importdecks(fileToRead1)
%IMPORTFILE(FILETOREAD1)
%  Imports data from the specified file
%  FILETOREAD1:  file to read

%  Auto-generated by MATLAB on 15-Jan-2016 07:55:40

% Import the file
tempData = importdata(fileToRead1);
for col=1:length(tempData.colheaders)
    decks.(tempData.colheaders{col})=tempData.data(:,col);
end
