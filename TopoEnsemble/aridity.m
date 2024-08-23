% aridity.m

%Takes about 3 seconds to run.

ncidANN=netcdf.open('amANN1490-1499.aijP215eoDOFP3Od_X008_O30.nc');
ncidANNname=('amANN1490-1499.aijP215eoDOFP3Od_X008_O30.nc');

fileID = fopen('fileaij.txt');
s = textscan(fileID,'%s');
fclose(fileID);
k_length=length(s{1});
ocnfr=ncread(s{1}{1},'ocnfr'); % read in ocean fraction (same in all input files) for this topography

%datetime('now')
% add path to penmont_vpd_model_20160701.m routine:
addpath(genpath('/home/mway/mway_input_files'));

% Inputs:
%       Ta      = mean daily temperature, degrees C         => tsurf "SURFACE AIR TEMPERATURE"
%       Tmax    = mean daily maximum temperature, degrees C => TMAXC "SURFACE AIR TEMPERATURE: DIURNAL HIGH"
%       Tmin    = mean daily minimum temperature, degrees C => TMINC "SURFACE AIR TEMPERATURE: DIURNAL LOW"
%       Rnet    = surface net radiation, W/m2    => srtrnf_grnd "NET RADIATION AT GROUND"
%       gflux   = ground heat flux, W/m2         => netht_land "NET HEAT INTO SOIL"
%       ea      = actual vapor pressure, Pascals => PVS "SURFACE VAPOR PRESSURE" **convert from mb**
%       press   = surface pressure, Pascals      => prsurf "SURFACE PRESSURE"    **convert from mb**
%       u2       = wind speed at 2 m , m/s       => wsurf "SURFACE WIND SPEED"
% NOTE: All ModelE surface variables at 10 m
%       r_surf  = surface resistance (bulk), s/m     => we specify instead of ModelE value (1/cond_can)
%       r_air   = aerodynamic resistance (bulk), s/m => we specify instead of ModelE value (1/cond_atm)
% Outputs:
%       PET  = potential evapotranspiration (mm/day)
%       VPD  = vapor presssure deficit (kPa)
%       RH   = relative humidity (fraction)

% Calculate Aridity using Michael Puma/Ben's Penman-Montieth index (over land) A=P/(P+PET) P=prec
% and place in the netcdf aij files.

%flag_veg=0 % we are using bare soil
kvonKarm = 0.41; % von Karman's constant, 0.41 [-],
zm = 10;         % height of wind measurements, m 
zh = 10;         % height of wind measurements, humidity m            

% elseif flag_veg == 0 % bare soil %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Bulk aerodynamic resistance
	veg_height = 0; % m
% Zero plane displacement
	d = 0;
% Roughness length governing momentum transfer
	z_om = 0.005;
% Roughness length governing transfer of heat and vapour
	z_oh = 0.005;
% Aerodynamic resistance: for neutral stability conditions, i.e., where
% temperature, atmospheric pressure, and wind velocity distributions follow
% nearly adiabatic conditions (no heat exchange). The application of the
% equation for short time periods (hourly or less) may require the
% inclusion of corrections for stability. However, when predicting ETo in
% the well-watered reference surface, heat exchanged is small, and
% therefore stability correction is normally not required

%% 'Bulk' surface resistance:
% r_surf => 0 when soil saturated assuming no crusting or
% other impediment to vapor transfer at soil surface
	r_surf = 0; 


for k=1:k_length
%for k=1:1
ncid{k}=netcdf.open(s{1}{k});

prec=ncread(s{1}{k},'prec');
var_Ta=ncread(s{1}{k},'tsurf');
var_Tmax=ncread(s{1}{k},'TMAXC');
var_Tmin=ncread(s{1}{k},'TMINC');
var_Rnet=ncread(s{1}{k},'srtrnf_grnd');
var_gflux=ncread(s{1}{k},'netht_land');
var_ea=ncread(s{1}{k},'PVS');
var_press=ncread(s{1}{k},'prsurf');
var_u2=ncread(s{1}{k},'wsurf');
r_air = (log((zm-d)./(z_om)).*log((zh-d)./(z_oh)))./(kvonKarm.^2.*var_u2);
[PET,PETenergy,PETvpd,VPD,RH] = penmont_vpd_model_20160701(var_Ta,var_Tmax,var_Tmin,var_Rnet,var_gflux,var_ea,var_press,r_surf,r_air);

% only calculate a when PET>=0 and ocnfr==0
a=prec./(prec+PET);
a(ocnfr~=0)=-1e+30; % replace all ocean values with -1e+30;
%a(PET<0.0)=-1e+30; % replace all PET<0.0 values with -1e+30;
a(PET<0.0)=NaN; % replace all PET<0.0 values with NaN so that we can sum w/o NaN values later.
%a(PET<0.0)=0; % replace all PET<0.0 values with 0 (necessary for when we sum later); NO, this just brings down the mean
% we need a way to calculate the mean w/o these values.
b(:,:,k)=a;

netcdf.close(ncid{k})
clear prec var_Ta var_Tmax var_Tmin var_Rnet var_gflux var_ea var_press var_u2 r_air PET PETenergy PETvpd VPD RH a;
end % k

c=mean(b,3,'omitnan');
d=c;
d(c==NaN)=-1e-30;
d(c<0)=-1e-30;
d(ocnfr~=0)=-1e+30;
d(d==NaN)=-1e-30;

nccreate(ncidANNname,'aridity','Dimensions',{'lon' 72 'lat' 46},'Format','classic','DataType','single');
ncwriteatt(ncidANNname,'aridity','missing_value',-1.000000015047466e+30);
ncwrite(ncidANNname,'aridity',d);
netcdf.close(ncidANN);

%datetime('now')
