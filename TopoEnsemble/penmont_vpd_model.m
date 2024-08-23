function [PET,PETenergy,PETvpd,VPD,RH] = penmont_vpd_model(Ta,Tmax,Tmin,Rnet,gflux,ea,press,r_surf,r_air)

% [PET,PETenergy,PETvpd,VPD,RH] = penmont_vpd_model(Ta,Tmax,Tmin,Rnet,gflux,ea,press,u,r_surf,r_air)
%
% Function to compute the full Penman-moneith equation (e.g. Equation 3 in
% Allen et al. [1988]). If Tmax and Tmin unavailable, then expect an
% underestimation of PET (see notes at end).
% 
% Inputs: (All values are at the specificed measurement height)
%       Ta      = mean daily temperature, degrees C         => tsurf "SURFACE AIR TEMPERATURE"
%       Tmax    = mean daily maximum temperature, degrees C => TMAXC "SURFACE AIR TEMPERATURE: DIURNAL HIGH"
%       Tmin    = mean daily minimum temperature, degrees C => TMINC "SURFACE AIR TEMPERATURE: DIURNAL LOW"
%       Rnet    = surface net radiation, W/m2    => srtrnf_grnd "NET RADIATION AT GROUND"
%       gflux   = ground heat flux, W/m2         => netht_land "NET HEAT INTO SOIL"
%       ea      = actual vapor pressure, Pascals => PVS "SURFACE VAPOR PRESSURE" **convert from mb**
%       press   = surface pressure, Pascals      => prsurf "SURFACE PRESSURE"    **convert from mb**
% NOTE: All ModelE surface variables at 10 m
%       r_surf  = surface resistance (bulk), s/m     => we specify instead of ModelE value (1/cond_can)
%       r_air   = aerodynamic resistance (bulk), s/m => we specify instead of ModelE value (1/cond_atm)
% Outputs:
%       PET  = potential evapotranspiration (mm/day)       
%       VPD  = vapor presssure deficit (kPa)       
%       RH   = relative humidity (fraction)
%
% Written by Michael J. Puma
% 
% Based on:
%       Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (1998). Crop
%       evapotranspiration-Guidelines for computing crop water
%       requirements-FAO Irrigation and drainage paper 56. FAO, Rome,
%       300(9), D05109. 
%       
%       Xu, C-Y., and V. P. Singh. "Cross comparison of empirical equations
%       for calculating potential evapotranspiration with data from
%       Switzerland." Water Resources Management, 16.3 (2002): 197-219.
%           
% Conversion factors
MJday_to_watts = 11.5740741; %1 megajoule per day = 11.5740741 watt

% Constants
R = 0.287058; % kJ kg-1 K-1
missing_value = -8e+28;

% Calculate the latent heat of vaporization (MJ kg-1) (FAO value 2.45)
lambda_lv=2.501-(2.361e-3).*Ta;

% Convert Pressure from Pa to kPa
press=press./1000;

% Convert actual vapor pressure from Pa to kPa
ea=ea./1000;

% Calculate saturation vapor pressure (kPa) => see notes at end
%es=0.611.*exp((17.27.*Ta)./(Ta+237.3));   
es_min=0.6108.*exp((17.27.*Tmin)./(Tmin+237.3));
es_max=0.6108.*exp((17.27.*Tmax)./(Tmax+237.3));
es = (es_min+es_max)/2;

% Slope of the vapor pressure curve (kPa C-1)
%delta_vpc=(4098.*es)./((Ta+237.3).^2);
delta_vpc=(4098.*(0.6108.*exp((17.27.*Ta)./(Ta+237.3))))./((Ta+237.3).^2);

% Specific heat of the air (MJ kg-1 °C-1)  => cp
specific_heat_air = 1.013e-3; % at constant pressure

% Mean air density at constant pressure (kg m-3) => rho_a
% using ideal gas law
density_air = press./(1.01.*(Ta+273)*R);

% Ratio molecular weight of water vapour/dry air = 0.622
epsilon = 0.622;

% Psychometric` parameter (kPa C-1)
%psych_parameter=0.00163.*(press./lambda_lv);
psych_parameter = (specific_heat_air.*press)./(epsilon.*lambda_lv);

% Net radiation
Rnet=Rnet./MJday_to_watts;   % convert W/m2 to MJ/m2/d

% Ground heat flux
gflux=gflux./MJday_to_watts; % convert W/m2 to MJ/m2/d
gflux(gflux<=missing_value) = 0;

% Calculate VPD
VPD=es-ea;

% Calculate Relative Humidity
RH = ea./es;


%% Components of evapotranspiration (mm)
PETenergy = (delta_vpc.*(Rnet-gflux)./lambda_lv)...
    ./(delta_vpc+psych_parameter.*(1+r_surf./r_air));

PETvpd    = (86400.*density_air.*specific_heat_air.*((es-ea)./(lambda_lv.*r_air)))...
            ./ ((delta_vpc+psych_parameter.*(1+r_surf./r_air)));

PET= PETenergy + PETvpd;



%% NOTES:
% Allen et al (1988) Chapter 3: "Using mean air temperature instead of
% daily minimum and maximum temperatures results in lower estimates for the
% mean saturation vapour pressure. The corresponding vapour pressure
% deficit (a parameter expressing the evaporating power of the atmosphere)
% will also be smaller and the result will be some underestimation of the
% reference crop evapotranspiration."








