function connectivity_avg_indNII

[Files,PathName,~] = uigetfile('*.nii','Please Select NIIs','MultiSelect','on');
cd(PathName);

for i = 1:length(Files)
    nii{i} = niftiread(Files{i});
    avg{i} = mean(nii{i}(nii{i}~=0));
end

[FileName,PathName,~] = uiputfile('*.xlsx','Save Your Average Values');
cd(PathName)
for i = 1:length(Files)
	concat_mat = cat(1,avg{:});
	xlswrite(FileName,concat_mat,Files{i}(1));
	clear concat_mat
end