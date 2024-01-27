function sourcespace_virtSens_findDominantOrientation(selection_method,concat)

%PURPOSE:           Find the dominant orientation from the timeseries of each participant, and export this timeseries in ".swf" format.
%
%REQUIRED INPUTS:   selection_method: 'ABSAVG', 'PEAKMAX', 'MAXAVG', 'MINAVG' specifies how the 
%                   dominant orientation should be determined (by the higher absolute average power over 
%                   the time window, the higher peak power within the time window, the higher average (not from absolute) power over the time window, 
%                   or the lowest average power (not from absolute) over the time window)
%
%                   concat: 'y' (optional), if this argument is found, concatenates the dominant orientation timeseries into a '.txt' file
%                   (this may significantly slow the function)
%		    
%
%NOTES:             (1)  Function will read any and all ".tfc" files within a directory with
%                        "Timeseries.tfc" in their title
%                   (2)  Concatenation function will read and order files in numerical order according to their titles!
%		   
%                  
%AUTHOR:            Alex I. Wiesman, DICoN Lab, University of Nebraska Medical Center
%VERSION HISTORY:   08/09/2016  v1: First working version of program
%                   02/25/2019  v2: Now saves a list of dominant
%                   orientations for each file
%                   03/05/2019  v3: Elizabeth Heinrichs-Graham edit: Included minimum
%                   orientations
%                   04/20/2022  v4: Fixed inproper indexing of the
%                   timestart and timeend indexes marked with "SDS"
              

%prompt for inputs and convert inputs to number format%
inputs = {'Time Start', 'Time End', 'Time Sampling Resolution', 'Epoch Start', 'Epoch End', 'Freq Start','Freq End'};
defaults = {'0', '0', '0', '0', '0', '0', '0'};	
answer = inputdlg(inputs, 'Please Input Parameters', 2, defaults, 'on');
[timestart,timeend,timesample,epochstart,epochend,freqstart,freqend] = deal(answer{:});
timestart =str2num(timestart);
timeend =str2num(timeend);
timesample =str2num(timesample);
epochstart =str2num(epochstart);
epochend =str2num(epochend);

%check that specified time window is not outside the range of the epoch%
if timestart < epochstart | timeend > epochend
	error('Disaster! Please select a time window within the length of the epoch!');
	fprintf('\n');
else
end

%find the cell references for the time window%
timestart = ((timestart-epochstart)/timesample)+1;
timeend = ((timeend-epochstart)/timesample)%+1; SDS


%read in files sequentially, and find their dominant orientation%
%depending on input, this will either select the orientation with the greatest peak or average amplitude/power within the time window% 
files = dir('*Timeseries.tfc');
numfiles = length(files);
if numfiles < 1
	error('No timeseries found in this location! That went well!');
	fprintf('\n');
end

dominant_orientations = zeros(1,length(files));
for i = 1:length(files)
	fprintf('Finding dominant orientation for file %.0f \n',i);
	[~,name,~] = fileparts(files(i).name);
    FileName = files(i).name;
            TFC = dlmread(FileName);
            TFC([1,4,9,12],:) = []; %Remove ori1 to ori2 ones
            TFC([5,6,7,8],:) = []; %Remove bottom half because redundant
    ori_timeseries = TFC;
	%ori_timeseries = dlmread(files(i).name,',');
    
	if strcmp(selection_method,'ABSAVG')
		ori1to1_Avg = abs(mean(ori_timeseries(1,timestart:timeend))); %Average each line
		ori1to2_Avg = abs(mean(ori_timeseries(2,timestart:timeend)));
        ori2to1_Avg = abs(mean(ori_timeseries(3,timestart:timeend)));
        ori2to2_Avg = abs(mean(ori_timeseries(4,timestart:timeend)));
        
        maxavg = [ori1to1_Avg, ori1to2_Avg, ori2to1_Avg, ori2to2_Avg]; %Create vector with each avg
        [q w] = max(maxavg); %determine highest number and save off
        Dom_Ori = ori_timeseries(w,:); %Call highest number
        dominant_orientations(1,i) = w;  %use this line as dominant orientation

	%elseif strcmp(selection_method,'PEAKMAX')
		%ori1_Max = max(abs(ori_timeseries(1,timestart:timeend)));
		%ori2_Max = max(abs(ori_timeseries(2,timestart:timeend)));

		%if ori1_Max > ori2_Max
		%	Dom_Ori = ori_timeseries(1,:);
        %    dominant_orientations(1,i) = 1;
		%elseif ori2_Max > ori1_Max
		%	Dom_Ori = ori_timeseries(2,:);
        %    dominant_orientations(1,i) = 2;
        %else
        %    fprintf('The orientations for this file were equivalent! Boooo ambiguity! \n')
        %end
        
    %elseif strcmp(selection_method,'MINAVG')
       % ori1_Max = mean(ori_timeseries(1,timestart:timeend));
       % ori2_Max = mean(ori_timeseries(2,timestart:timeend));

       % if ori1_Max < ori2_Max
        %    Dom_Ori = ori_timeseries(1,:);
        %    dominant_orientations(1,i) = 1;
        %elseif ori2_Max < ori1_Max
        %    Dom_Ori = ori_timeseries(2,:);
        %    dominant_orientations(1,i) = 2;
        %else
         %   fprintf('The orientations for this file were equivalent! Boooo ambiguity! \n')
        %end
        
    %elseif strcmp(selection_method,'MAXAVG')
       % ori1_Max = mean(ori_timeseries(1,timestart:timeend));
       % ori2_Max = mean(ori_timeseries(2,timestart:timeend));

       % if ori1_Max > ori2_Max
       %     Dom_Ori = ori_timeseries(1,:);
       %     dominant_orientations(1,i) = 1;
       % elseif ori2_Max > ori1_Max
       %     Dom_Ori = ori_timeseries(2,:);
       %     dominant_orientations(1,i) = 2;
       % else
       %     fprintf('The orientations for this file were equivalent! Boooo ambiguity! \n')
       % end
        
   % else error('Aw Geez! The selection_method specified is not valid!')
       % fprintf('\n');
   
    
    %build the SWF heading, and write the file%
    numpoints = length(ori_timeseries);
    TSB = epochstart;
    DI = timesample;
    header = sprintf('Npts= %.0f TSB= %.0f DI= %.0f SB= 1.000 SC= 200.0 CondFile= "timeseries_dominant_orientation.fif" CondName= "Dominant_orientation" Filter= "%s - %s Hz  NF:60"', numpoints, TSB, DI, freqstart, freqend);
    filename = strcat(name,'_sourcespace_virtSens_findDominantOrientation','.swf');
    fid = fopen(filename,'wt');
    fprintf(fid,'%s\n',header);
    fprintf(fid,'%s','Dom-Ori:   ');   
    fclose(fid);
    dlmwrite(filename,Dom_Ori,'delimiter',' ','-append');

    %if instructed by the 'concat' argument, concatenate the dominant orientation
    %timeseries and average them in the time domain%
    if nargin == 2 & strcmp(concat,'y')
        if i == 1
            Dominant_Ori_Concat = Dom_Ori;
        elseif i > 1
            Dominant_Ori_Concat = [Dominant_Ori_Concat; Dom_Ori];
        end

        Dominant_Ori_Concat_Average = mean(Dominant_Ori_Concat);
    end



end


if nargin == 2 & strcmp(concat,'y')
    dlmwrite('Concatentated_DomOri_Timeseries.txt',Dominant_Ori_Concat,'delimiter','\t','precision',4)
    dlmwrite('Averaged_DomOri_Timeseries.txt',Dominant_Ori_Concat_Average,'delimiter','\t','precision',4)
end





fid = fopen('Dominant Orientations.txt','wt');
fprintf(fid,'%s\t %s\n','File','Dominant Orientation');       %write dominant orientation data to new file





for i = 1:length(files)
    fprintf(fid,'%s\t %d\n',files(i).name,dominant_orientations(1,i));
end





fclose(fid);

fprintf('Perfectenschlag! %.0f timeseries converted! \n',numfiles);
end


