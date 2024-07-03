// Bounding Box of Wilma Breeding Area
var geometry = ee.Geometry.Rectangle([3.961857, 49.875739, 4.430933, 50.106059]);

var startDate = '2023-04-04';
var endDate = '2023-07-16';

// Load the Landsat 9 collection and filter by date and region
var landsatCollection = ee.ImageCollection('LANDSAT/LC09/C02/T1_L2')
.filterDate(startDate, endDate)
.filterBounds(geometry);

// Function to calculate NDVI
var calculateNDVI = function(image) {
  var ndvi = image.normalizedDifference(['SR_B5', 'SR_B4']).rename('NDVI');
  return image.addBands(ndvi);
};

// Apply the NDVI function to the Landsat collection
var landsatNDVI = landsatCollection.map(calculateNDVI);

// Reduce the collection to get the median NDVI value for the period
var medianNDVI = landsatNDVI.select('NDVI').median().clip(geometry);

// Define visualization parameters
var colorizedVis = {
  min: 0,
  max: 1,
  palette: [
    'ffffff', 'ce7e45', 'df923d', 'f1b555', 'fcd163', '99b718', '74a901',
    '66a000', '529400', '3e8601', '207401', '056201', '004c00', '023b01',
    '012e01', '011d01', '011301'
  ],
};

// Center the map on the region of interest and add the NDVI layer
Map.setCenter(4.196, 49.991, 10);
Map.addLayer(medianNDVI, colorizedVis, 'Median NDVI');

// Export the result to Google Drive
Export.image.toDrive({
  image: medianNDVI,
  description: 'Wilma_Breeding_Median_NDVI_April_to_July_2023',
  scale: 30,
  region: geometry,
  fileFormat: 'GeoTIFF'
});

